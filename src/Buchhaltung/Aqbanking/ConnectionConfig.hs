{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Buchhaltung.Aqbanking.ConnectionConfig
Description : Configuration of a single online banking connection
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Configuration parameters for a connection to a bank (login, password, etc.).
-}
module Buchhaltung.Aqbanking.ConnectionConfig where

import Buchhaltung.Config
import Data.Aeson
import Data.Text as T
import GHC.Generics
import Protolude
import System.Directory
import System.FilePath

class CLIArg a where
  toArg :: a -> Text

data ConnectionConfig
  = ConnectionConfig
      { name :: Text,
        -- ^ The local name for this connection
        blz :: Text,
        url :: Text,
        login :: Text,
        -- ^ The login name to be used when connecting
        hbciv :: HBCIVersion,
        typ :: AqType,
        -- ^ The type of authentication to use
        parent :: Config
        -- ^ The parent configuration, that is, the 'Buchhaltung.Config' this is
        -- part of. This is used, for example, to generate file paths etc.
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON ConnectionConfig

instance FromJSON ConnectionConfig

data AqType
  = PinTan
  | Other
  deriving (Eq, Generic, Ord, Show)

instance ToJSON AqType

instance FromJSON AqType

instance CLIArg AqType where
  toArg PinTan = "pintan"

data HBCIVersion
  = HBCI201
  | HBCI210
  | HBCI220
  | HBCI300
  deriving (Eq, Generic, Ord, Show)

instance ToJSON HBCIVersion

instance FromJSON HBCIVersion

instance CLIArg HBCIVersion where
  toArg HBCI201 = "201"
  toArg HBCI210 = "210"
  toArg HBCI220 = "220"
  toArg HBCI300 = "300"

{-|
The file path of the directory to save data related to the given connection in.
-}
-- TODO Make sure at top config level that this is unique
configDir :: ConnectionConfig -> FilePath
configDir conf =
  makeValid
    $ path (parent conf) </> T.unpack (name conf)

{-|
The file path of the aqbanking context file that should be used for the given
connection.
-}
contextFile :: ConnectionConfig -> FilePath
contextFile conf = configDir conf <> ".ctx"
