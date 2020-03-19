{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Main entry point
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

This is the main entry point of the @aq2ledger@ executable.
-}
module Main where

import Aq2Ledger.AqBanking
import Aq2Ledger.AqBanking.Request
import Aq2Ledger.Hledger
import Aq2Ledger.Options
import Aq2Ledger.Parse
import Aq2Ledger.Prelude hiding (option)
import Data.Either.Extra (mapLeft)
import qualified Data.Text as T
import Data.Time (addDays)
import Data.Time.Calendar (Day)
import qualified Data.Yaml as Y
import Hledger.Data hiding (Account)
import Options.Applicative hiding (ParseError)
import Text.Parsec hiding (option, optional)

main :: IO ()
main = processOptions =<< execParser options'
  where
    options' =
      info (options <**> helper)
        ( fullDesc
            <> progDesc "Wrapper around AqBanking"
            <> header
                 "aq2ledger – gettin' your online banking stuff into (H)Ledger"
        )

-- TODO Collect all NonFatal errors and print them at the end in a summary
processOptions :: Options -> IO ()
processOptions ExampleConfig =
  putText . decodeUtf8 . encode $ (def :: Config)
processOptions (Download from to confFile' (Just nam)) = do
  conf <- getConfig confFile'
  void . runAq conf $ downloadTxs from to nam
processOptions (Download from to confFile' Nothing) = do
  conf <- getConfig confFile'
  sequence_
    $ forAllCons conf
    $ runAq conf . downloadTxs from to
processOptions (Print from to confFile' (Just nam)) = do
  conf <- getConfig confFile'
  void . runAq conf $ printTxs from to nam
processOptions (Print from to confFile' Nothing) = do
  conf <- getConfig confFile'
  sequence_
    $ forAllCons conf
    $ runAq conf . printTxs from to

{-|
Maps over all connections in the given configuration, collecting results in a
list.
-}
forAllCons :: Config -> (ConnectionName -> b) -> [b]
forAllCons conf = (<$> (name <$> connections conf))

{-|
Downloads the transactions of the connection (given by the connection name) that
happened between the two dates, storing them in the connection-specific CTX
file. If the second date is left out, it is treated as if today's date was
given.

Automatically repeats this action using the date of the last transaction stored
in the connection-specific CTX file until no new transactions are added anymore.
This counteracts some banks' strange behaviour (i.e. DKB's) that only allows the
download of a (sometimes non-deterministic) number of transactions.
-}
downloadTxs
  :: Day
  -> Maybe Day
  -> ConnectionName
  -> Aq ()
downloadTxs from toM nam = do
  t <- parseLocalTxs nam
  downloadTxs' from toM nam t

downloadTxs'
  :: Day
  -> Maybe Day
  -> ConnectionName
  -> [Transaction]
  -> Aq ()
downloadTxs' from toM nam t = do
  conf <- ask
  to <- maybe today return toM
  getTransactions nam from to
  t' <- parseLocalTxs nam
  when (t' /= t)
    $ let from' = maybe (addDays 1 from) tdate (lastMay t')
       in downloadTxs' from' toM nam t'

{-|
Prints the transactions of the connection given by the connection name that
happened between the two dates. If the second date is left out, it is treated as
“today”.
-}
printTxs
  :: Day
  -> Maybe Day
  -> ConnectionName
  -> Aq ()
printTxs from to nam = do
  conf <- ask
  sequence_ $ (<$> connections conf) $ \con -> do
    t <- parseLocalTxs nam
    sequence_
      $ putStrLn . showTransaction
        <$> restrictTxs from to t

{-|
Parses the transactions stored locally in the CTX file of the connection
specified by the given name.
-}
parseLocalTxs :: ConnectionName -> Aq [Transaction]
-- TODO Make this fail non-fatally
parseLocalTxs nam = do
  conf <- ask
  -- TODO Deal with this weird line
  s <- localTransactions nam
  -- `catchError` (\e -> do (putText . show) e; return "")
  either throwError return
    $ ($ accountNameMap conf)
      <$> mapLeft tagError
            (parse (listtrans fromIBANDE) "" (T.unpack s))
  where
    tagError =
      AqNonFatal
        . ("Parsing `aqbanking-cli listtrans` output failed: " <>)
        . show

{-|
There are two kinds of errors. Ones that lead to aborting everything (e.g.
config couldn't be parsed) and ones that only lead to aborting something (these
get put into a summary at the end).
-}
newtype A2LException = A2LFatal Text
  deriving (Show)

instance Exception A2LException

{-|
Gets the config, either from the given file or from its default location.
-}
getConfig :: Maybe FilePath -> IO Config
getConfig confFile' = do
  confFile <- liftIO $ maybe defaultConfigFile return confFile'
  confE <- readConfigFile confFile
  case confE of
    Left err ->
      throwIO
        . A2LFatal
        $ "Config parsing error: " <> T.pack err
    Right conf -> return conf

{-|
Reads from disk and parses the given config file.

An alternative implementation may be

> readConfigFile :: (MonadIO m, MonadError Text m) => FilePath -> m Config
> readConfigFile =
>   ((liftEither . first (T.pack . Y.prettyPrintParseException)) =<<)
>     . liftIO
>     . Y.decodeFileEither
-}
readConfigFile :: FilePath -> IO (Either String Config)
readConfigFile = fmap (first Y.prettyPrintParseException) . Y.decodeFileEither
