{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Buchhaltung.Aqbanking
Description : Interface for aqbanking executables
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Abstract monadic interface to run aqbanking executables; paths are retrieved
from a 'Buchhaltung.Config.Config' in a 'ReaderT'.
-}
module Buchhaltung.Aqbanking
  ( module Buchhaltung.Aqbanking,
    module Buchhaltung.Config,
  )
where

import Buchhaltung.Config
import Buchhaltung.Prelude
import qualified Data.Text as T
import System.Process (callProcess, readProcess)

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
runAq :: Aq a -> Config -> IO (Either Text a)
runAq aq = runReaderT (runExceptT . unAq $ aq)

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
'Buchhaltung.Config' and returns its standard output as 'Data.Text'. Thus,

@
read aqhbciExe []
@

is an 'Aq' action that runs the configured @aqhbci-tool4@ executable without
arguments and returns its standard output.

We use 'String's for command names and arguments because 'System.Process' only
accepts those.
-}
read
  :: (MonadIO m, MonadReader Config m)
  => ConnectionConfig
  -> (Config -> String)
  -> [String]
  -> m Text
read conn prog args = do
  cmd <- asks prog
  fmap T.pack . liftIO
    $ readProcess cmd (["-D", path conn] <> args) mempty

{-|
Like 'read' but only runs the command (without returning its standard output as
'Text' object).
-}
run
  :: (MonadIO m, MonadReader Config m)
  => ConnectionConfig
  -> (Config -> String)
  -> [String]
  -> m ()
run conn prog args = do
  cmd <- asks prog
  liftIO $ callProcess cmd (["-D", path conn] <> args)
