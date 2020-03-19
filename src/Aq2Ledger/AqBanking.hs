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
  = AqFatal Text
  | AqNonFatal Text
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
  :: ConnectionConfig
  -> (Config -> String)
  -> [String]
  -> Aq Text
read conn prog args = do
  cmd <- asks prog
  (exitCode, stdout, stderr) <-
    liftIO
      $ readProcessWithExitCode cmd (["-D", path conn] <> args) mempty
  if exitCode /= ExitSuccess
    then throwError . AqNonFatal $ T.pack stderr
    else return $ T.pack stdout

{-|
Like 'read' but only runs the command in a fire-and-forget way (doesn't return
or check any of the command's exit code, stdout or stderr) providing it with an
empty stdin (see 'interact' for the non-empty stdin).
-}
run
  :: ConnectionConfig
  -> (Config -> String)
  -> [String]
  -> Aq ()
run conn prog args = do
  cmd <- asks prog
  void . liftIO $ readProcessWithExitCode cmd (["-D", path conn] <> args) mempty

{-|
Like 'read' but runs the command and allows to interact with it (i.e. nothing is
piped into stdin).
-}
interact
  :: ConnectionConfig
  -> (Config -> String)
  -> [String]
  -> Aq ()
interact conn prog args = do
  cmd <- asks prog
  void . liftIO $ callProcess cmd (["-D", path conn] <> args)

{-|
Today's date.
-}
today :: Aq Day
today = liftIO $ utctDay <$> getCurrentTime
