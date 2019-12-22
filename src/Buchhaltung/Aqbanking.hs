{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Buchhaltung.Aqbanking
Description : TODO
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

TODO
-}
module Buchhaltung.Aqbanking
  ( module Buchhaltung.Aqbanking,
    module Buchhaltung.Aqbanking.ConnectionConfig,
    module Buchhaltung.Config,
  )
where

import Buchhaltung.Aqbanking.ConnectionConfig
import Buchhaltung.Config
import Buchhaltung.Prelude
import Data.Text as T
import System.Process (callProcess, readProcess)

-- newtype Aq a = Aq {unAq :: ReaderT Config IO a}
--   deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)
type Aq a = ReaderT ConnectionConfig IO a

{-|
Runs the command with the supplied arguments using the first argument to
extract the command to run (i.e., the file path to its executable etc.) from
'Buchhaltung.Config' and returns its standard output as 'Data.Text'. Thus,

@
read aqhbciExe []
@

is an 'Aq' action that runs the configured @aqhbci-tool4@ executable without
arguments and returns its standard output.

We use 'String's because 'System.Process' only deals with those.
-}
read :: (Config -> String) -> [String] -> Aq Text
read prog args = do
  cmd <- asks (prog . parent)
  dir <- asks configDir
  fmap T.pack . lift
    $ readProcess cmd (["-D", dir] <> args) mempty

{-|
Like 'read' but only runs the command (without returning its standard output as
'Text' object).
-}
run :: (Config -> String) -> [String] -> Aq ()
run prog args = do
  cmd <- asks (prog . parent)
  dir <- asks configDir
  lift $ callProcess cmd (["-D", dir] <> args)
