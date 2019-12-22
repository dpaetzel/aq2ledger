{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Buchhaltung.Config
Description : TODO
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

TODO
-}
module Buchhaltung.Config where

import Buchhaltung.Prelude
import Data.Aeson

class Default a where
  def :: a

data Config
  = Config
      { aqhbciExe :: String,
        -- ^ We use 'String' because 'System.Process' only deals with those.
        aqbankingExe :: String,
        -- ^ We use 'String' because 'System.Process' only deals with those.
        path :: FilePath
      }
  deriving (Eq, Generic, Ord, Show)

instance Default Config where
  def = Config
    { aqhbciExe = "aqhbci-tool4",
      aqbankingExe = "aqbanking-cli",
      path = "buchhaltung-config"
    }

instance ToJSON Config

instance FromJSON Config
