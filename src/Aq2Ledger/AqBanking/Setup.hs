{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Aq2Ledger.AqBanking.Setup
Description : TODO
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

TODO

For now, we require the user to set up AqBanking manually because there are too
many difference between banks.

See https://www.aquamaniac.de/rdm/projects/aqbanking/wiki/SetupPinTan .
-}
module Aq2Ledger.AqBanking.Setup where

import Aq2Ledger.AqBanking
import Data.Yaml
import Protolude

{-|
Data types that are can be converted to CLI arguments.
-}
class CLIArg a where
  toArg :: a -> Text

{-|
Possible types of authentication; to be used in automated connection setup.
-}
data AqType
  = PinTan
  | Other
  deriving (Eq, Generic, Ord, Show)

instance ToJSON AqType

instance FromJSON AqType

instance CLIArg AqType where
  toArg PinTan = "pintan"

{-|
Usable versions of HBCI; to be used in automated connection setup.
-}
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
Configuration of a connection to a bank.
-}
data ConnectionSetup
  = ConnectionSetup
      { cname :: ConnectionName,
        -- ^ The 'Config'-wide unique(!) name for this connection.
        bankCode :: Text,
        url :: Text,
        login :: Text,
        -- ^ The login name to be used when connecting.
        hbciv :: HBCIVersion,
        -- ^ The HBCI version to use when setting up this connection.
        typ :: AqType,
        -- ^ The type of authentication to use when setting up this connection.
        cpath :: FilePath
        -- ^ File path to use for storing data associated with this connection.
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON ConnectionSetup

instance FromJSON ConnectionSetup

type ID = Text

type Flag = Text

type Command = Text

addUser = undefined

-- do
-- conf <- ask
-- run aqhbciExe
--   [ "adduser",
--     "-N",
--     name conf,
--     "-b",
--     bankCode conf,
--     "-u",
--     login conf,
--     "-t",
--     toArg . typ $ conf,
--     "-s",
--     url conf,
--     -- "--context=1",
--     "--hbciversion=" <> toArg (hbciv conf)
--   ]
getUniqueID :: ConnectionSetup -> Aq ID
getUniqueID = undefined

withID :: ConnectionSetup -> (ID -> Aq a) -> Aq a
withID = undefined
-- adduserflags -u UniqueId -f tlsIgnPrematureClose
-- getbankInfo -u UniqueId
-- getsysid -u UniqueId
-- optional: listitanmodes -u UniqueId
-- optional: setitanmode -u UniqueId -m 6930
-- optional: aqhbci-tool4 setTanMediumId -u UniqueId -m "TAN_MEDIUM_KENNUNG"
-- aqhbci-tool4 getaccounts -u UniqueId
-- aqhbci-tool4 listaccounts -v -- get LocalUniqueId here
-- optional (in case of error during transaction request)
-- aqhbci-tool4 getaccsepa -a LocalUniqueId -- for each account
-- optional, to test the setup: aqbanking-cli request --account=Kontonummer --fromdate=20190913 --transactions
