{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Buchhaltung.BankAccount
Description : Bank account identifiers
Copyright   : David Pätzel, 2020
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

A simple data type for identifying bank accounts.
-}
module Buchhaltung.BankAccount where

import Buchhaltung.Prelude
import Data.Yaml

data BankAccount
  = BankAccount
      { bank :: String,
        account :: String
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON BankAccount

instance FromJSON BankAccount
