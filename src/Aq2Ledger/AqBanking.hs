{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Aq2Ledger.AqBanking
Description : Interface for AqBanking executables
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Abstract monadic interface to run AqBanking executables; paths are retrieved
from a 'Aq2Ledger.Config.Config' in a 'ReaderT'.
-}
module Aq2Ledger.AqBanking
  ( module Aq2Ledger.AqBanking,
    module Aq2Ledger.Config,
  )
where

import Aq2Ledger.Config
import Aq2Ledger.Prelude
import Data.List (unwords)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time (Day, getCurrentTime, utctDay)
import System.Console.ANSI
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Process.Typed

{-|
Monad stack representing AqBanking computations.
-}
newtype Aq a = Aq {unAq :: ExceptT AqException (ReaderT Config IO) a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadError AqException,
      MonadIO,
      MonadReader Config
    )

{-|
Helper function to run @Aq@ computations.
-}
runAq :: Config -> Aq a -> IO (Either AqException a)
runAq conf aq = runReaderT (runExceptT . unAq $ aq) conf

-- TODO Generalize: NonFatal always if there is a ConnectionName argument
{-|
Exceptions thrown when interacting with AqBanking.

Fatal exceptions lead to @aq2ledger@ not being able to continue at all while
non-fatal exceptions only lead to aborting processing the connection currently
being processed.
-}
data AqException
  = AqFatal
      { msg :: Text
      }
  | AqNonFatal
      { msg :: Text
      }
  deriving (Show)

instance Exception AqException

{-|
Tries to extract the subconfiguration of the connection of the given name from
the configuration.
-}
connection
  :: Text
  -> Aq ConnectionConfig
connection nam = do
  con <- asks (connection' nam)
  maybe noConConfError return con
  where
    connection' :: Text -> Config -> Maybe ConnectionConfig
    connection' nam = find ((nam ==) . name) . connections
    noConConfError =
      throwError . AqNonFatal
        $ "No connection configured with name '" <> nam <> "'"

{-|
Outputs an info message in a positive colour.
-}
putInfo :: Text -> Aq ()
putInfo msg = do
  liftIO $ hSetBuffering stdout NoBuffering
  liftIO $ setSGR [SetColor Foreground Vivid Green]
  putStrLn msg
  liftIO $ setSGR [Reset]

{-|
Runs the command with the supplied arguments using the first argument to
extract the command to run (i.e., the file path to its executable etc.) from
'Aq2Ledger.Config' and returns its standard output as 'Data.Text'. Thus,

@
read aqhbciExe []
@

is an 'Aq' action that runs the configured @aqhbci-tool4@ executable without
arguments and returns its standard output.

We use 'String's for command names and arguments because 'System.Process.Typed'
only accepts those.

If the created process's exit code indicates failure, throws an exception
containing its stdout and stderr.
-}
read
  :: ConnectionConfig
  -> (Config -> String)
  -> [String]
  -> Aq Text
read conn prog args = do
  cmd <- asks prog
  (stdout, _) <- readProcess_ (proc cmd (["-D", path conn] <> args))
  -- TODO Is it really OK to use `toStrict` here?
  return . toStrict . TL.decodeUtf8 $ stdout

{-|
Like 'read' but runs the command and allows to interact with it (i.e. stdin is
not fixed).

Other than 'read', while this does throw an exception if the created process's
exit code indicates failure as well, it does not containing the process's stdout
and stderr (otherwise proper user interaction is hindered by e.g. prompts being
output after the user enters input).
-}
-- NOTE I've looked into using pipes to properly handle prompts but that didn't
-- work out straightforwardly either.
interact
  :: ConnectionConfig
  -> (Config -> String)
  -> [String]
  -> Aq ()
interact conn prog args = do
  cmd <- asks prog
  putInfo
    . T.pack
    . ("Interacting with " <>)
    . unwords
    . fmap (wrap "'")
    $ [cmd, "-D", path conn] <> args
  -- TODO aqbanking-cli's exit code is positive even if the user aborts the
  -- dialog leading to no exception being thrown here. This could only be
  -- detected by analysing stdout/stderr …
  runProcess_ (proc cmd (["-D", path conn] <> args))

{-|
Wraps its second argument by its first argument.
-}
wrap :: Monoid m => m -> m -> m
wrap c x = c <> x <> c

{-|
Today's date.
-}
today :: Aq Day
today = liftIO $ utctDay <$> getCurrentTime
