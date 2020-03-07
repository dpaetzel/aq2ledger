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
-- (defaultTimeLocale, parseTimeM)
import Aq2Ledger.Options
import Aq2Ledger.Parse
import Aq2Ledger.Prelude hiding (option)
import Data.Either.Extra (mapLeft)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import qualified Data.Yaml as Y
import Hledger.Data hiding (Account)
import Options.Applicative
import TestConnections
import Text.Parsec hiding (option, optional)

main :: IO ()
main = processOptions =<< execParser options'
  where
    options' =
      info (options <**> helper)
        ( fullDesc
            <> progDesc "Wrapper around AqBanking"
            <> header
                 "aq2ledger – gettin' your online banking stuff into (H)ledger"
        )

processOptions :: Options -> IO ()
processOptions ExampleConfig =
  putText . decodeUtf8 . encode $ (def :: Config)
processOptions opts = do
  let con = testConnectionKSK2
  case opts of
    Download {} ->
      putText "Download not yet supported by CLI"
    Print from to confFile nam -> printTxs from to confFile nam

printTxs :: Day -> Maybe Day -> Maybe FilePath -> Maybe ConnectionName -> IO ()
printTxs from to confFile' nam = do
  confFile <- maybe defaultConfigFile return confFile'
  conf' <- readConfigFile confFile
  case conf' of
    Left err -> putStrLn err
    Right conf ->
      sequence_ $ (<$> connections conf) $ \con -> do
        sE <- runAq (localTransactions con) conf
        let t = do
              s <- sE
              mapLeft show $ parse (listtrans fromIBANDE) "" (T.unpack s)
        case t of
          Left err -> putText err
          Right t ->
            sequence_
              $ putStrLn . showTransaction
                <$> (restrictTxs from to . t $ accountNameMap conf)

{-|
Parses the given config file.

An alternative implementation may be

> readConfigFile :: (MonadIO m, MonadError Text m) => FilePath -> m Config
> readConfigFile =
>   ((liftEither . first (T.pack . Y.prettyPrintParseException)) =<<)
>     . liftIO
>     . Y.decodeFileEither
-}
readConfigFile :: FilePath -> IO (Either String Config)
readConfigFile = fmap (first Y.prettyPrintParseException) . Y.decodeFileEither
