{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Aq2Ledger.Config
Description : Configuration data types
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Configuration for the @aq2ledger@ executable.
-}
module Aq2Ledger.Config
  ( module Aq2Ledger.Config,
    Data.Yaml.decode,
    Data.Yaml.encode,
  )
where

import Aq2Ledger.BankAccount
import Aq2Ledger.Prelude
import Data.Aeson
import Data.List (lookup)
import qualified Data.Text as T
import Data.Yaml
import Hledger.Data (AccountName)
import System.FilePath

{-|
Data types with default values.
-}
class Default a where
  def :: a

{-|
Configuration data type for the @aq2ledger@ executable.
-}
data Config
  = Config
      { aqhbciExe :: String,
        -- ^ File path or name of the @aqhbci-tool4@ executable. We use 'String'
        -- because 'System.Process' only deals with those.
        aqbankingExe :: String,
        -- ^ File path or name of the @aqbanking-cli@ executable. We use
        -- 'String' because 'System.Process' only deals with those.
        accounts :: [(BankAccount, AccountName)],
        -- ^ Mapping of bank accounts to account names used in Hledger journal.
        connections :: [ConnectionConfig]
        -- ^ Configurations of connections to banks.
      }
  deriving (Eq, Generic, Ord, Show)

instance Default Config where
  def = Config
    { aqhbciExe = "aqhbci-tool4",
      aqbankingExe = "aqbanking-cli",
      accounts =
        [ (BankAccount "99999999" "123456789", "Bank:Personal"),
          (BankAccount "88888888" "987654321", "Bank:Secondary")
        ],
      connections =
        [ ConnectionConfig
            { name = "1",
              path = "connection1"
            }
        ]
    }

instance ToJSON Config

instance FromJSON Config

{-|
Transforms the given 'Config''s accounts mapping to a function which returns
@"TODO"@ for any bank accounts not mapped to a name.
-}
accountNameMap :: Config -> BankAccount -> AccountName
accountNameMap conf x =
  fromMaybe "TODO"
    . lookup x
    $ accounts conf

type ConnectionName = Text

{-|
Configuration of a connection to a bank.

NOTE: Since we don't support setting up connections yet, the following fields
are not included for now (but probably will be in a future version):

> bankCode :: Text
> url :: Text
> login :: Text
> -- ^ The login name to be used when connecting.
> hbciv :: HBCIVersion
> -- ^ The HBCI version to use when setting up this connection.
> typ :: AqType
> -- ^ The type of authentication to use when setting up this connection.
-}
data ConnectionConfig
  = ConnectionConfig
      { name :: ConnectionName,
        -- ^ The 'Config'-wide unique(!) name for this connection.
        path :: FilePath
        -- ^ File path to use for storing data associated with this connection.
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON ConnectionConfig

instance FromJSON ConnectionConfig

{-|
The file path of the aqbanking context file that should be used for the given
connection.
-}
contextFile :: ConnectionConfig -> FilePath
contextFile conf = path conf </> T.unpack (name conf) <> ".ctx"
