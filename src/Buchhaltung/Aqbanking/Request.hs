{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Buchhaltung.Aqbanking.Request
Description : Reading local and remote transactions
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Reading local CTX files and downloading transactions to them.
-}
module Buchhaltung.Aqbanking.Request where

import Buchhaltung.Aqbanking
import Buchhaltung.Config
import Buchhaltung.Format (listtransFormat)
import Buchhaltung.Prelude
import Data.Text as T

{-|
Uses the @listtrans@ subcommand to list all local transactions for the
'ConnectionConfig' currently selected.
-}
localTransactions :: Aq Text
localTransactions = do
  conf <- ask
  read aqbankingExe
    [ "listtrans",
      "--ctxfile=" <> contextFile conf,
      "--template=" <> listtransFormat
    ]

{-|
Uses the @export@ subcommand to list local transactions.

This seems to work not as stably as the @listtrans@ subcommand used by
'localTransaction' in that the account numbers are sometimes lacking (e.g. for
Kreissparkasse connections)?
-}
localTransactions' :: Aq Text
localTransactions' = do
  conf <- ask
  read aqbankingExe
    [ "export",
      "--ctxfile=" <> contextFile conf
    ]

{-|
Uses the @request@ subcommand to retrieve all transactions between the given
dates.
-}
getTransactions :: String -> String -> Aq ()
getTransactions from to = do
  conf <- ask
  run aqbankingExe
    [ "request",
      "--transactions",
      "--ignoreUnsupported",
      "--fromdate=" <> from,
      "--todate=" <> to,
      "--ctxfile=" <> contextFile conf
    ]
