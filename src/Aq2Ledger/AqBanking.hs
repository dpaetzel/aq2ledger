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
import qualified Data.Text as T
import Data.Time (Day, getCurrentTime, utctDay)
import System.Process (callProcess, readProcessWithExitCode)

{-|
Monad stack representing AqBanking computations.
-}
newtype Aq a = Aq {unAq :: ExceptT Text (ReaderT Config IO) a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadError Text,
      MonadIO,
      MonadReader Config
    )

{-|
Helper function to run @Aq@ computations.
-}
runAq :: Config -> Aq a -> IO (Either Text a)
runAq conf aq = runReaderT (runExceptT . unAq $ aq) conf

{-|
Tries to extract the subconfiguration of the connection of the given name from
the configuration.
-}
connection
  :: (MonadError Text m, MonadReader Config m)
  => Text
  -> m ConnectionConfig
connection nam = do
  conn <- asks (connection' nam)
  case conn of
    Just conn -> return conn
    Nothing ->
      throwError $ "No connection configured with name '" <> nam <> "'"
  where
    connection' :: Text -> Config -> Maybe ConnectionConfig
    connection' nam = find ((nam ==) . name) . connections

{-|
Runs the command with the supplied arguments using the first argument to
extract the command to run (i.e., the file path to its executable etc.) from
'Aq2Ledger.Config' and returns its standard output as 'Data.Text'. Thus,

@
read aqhbciExe []
@

is an 'Aq' action that runs the configured @aqhbci-tool4@ executable without
arguments and returns its standard output.

We use 'String's for command names and arguments because 'System.Process' only
accepts those.
-}
read
  :: (MonadError Text m, MonadIO m, MonadReader Config m)
  => ConnectionConfig
  -> (Config -> String)
  -> [String]
  -> m Text
read conn prog args = do
  cmd <- asks prog
  (exitCode, stdout, stderr) <-
    liftIO
      $ readProcessWithExitCode cmd (["-D", path conn] <> args) mempty
  if exitCode /= ExitSuccess
    then throwError $ T.pack stderr
    else return $ T.pack stdout

{-|
Like 'read' but only runs the command in a fire-and-forget way (doesn't return
or check any of the command's exit code, stdout or stderr) providing it with an
empty stdin (see 'interact' for the non-empty stdin).
-}
run
  :: (MonadIO m, MonadReader Config m)
  => ConnectionConfig
  -> (Config -> String)
  -> [String]
  -> m ()
run conn prog args = do
  cmd <- asks prog
  _ <- liftIO $ readProcessWithExitCode cmd (["-D", path conn] <> args) mempty
  return ()

{-|
Like 'read' but runs the command and allows to interact with it (i.e. nothing is
piped into stdin).
-}
interact
  :: (MonadIO m, MonadReader Config m)
  => ConnectionConfig
  -> (Config -> String)
  -> [String]
  -> m ()
interact conn prog args = do
  cmd <- asks prog
  _ <- liftIO $ callProcess cmd (["-D", path conn] <> args)
  return ()

{-|
Today's date.
-}
today :: Aq Day
today = liftIO $ utctDay <$> getCurrentTime
