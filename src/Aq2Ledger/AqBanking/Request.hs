{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO Add log messages
{-|
Module      : Aq2Ledger.AqBanking.Request
Description : Retrieving local and remote transactions
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Reading local CTX files and downloading transactions to them.
-}
module Aq2Ledger.AqBanking.Request where

import Aq2Ledger.AqBanking
import Aq2Ledger.Format (asAqDate, listtransFormat)
import Aq2Ledger.Prelude
import Data.Time

{-|
Uses the @listtrans@ subcommand to list all local transactions for the
'ConnectionConfig' currently selected.
-}
localTransactions :: ConnectionConfig -> Aq Text
localTransactions conn =
  read conn aqbankingExe
    [ "listtrans",
      "--ctxfile=" <> contextFile conn,
      "--template=" <> listtransFormat
    ]

{-|
Uses the @export@ subcommand to list local transactions.

This seems to work not as stably as the @listtrans@ subcommand used by
'localTransactions' in that the account numbers are sometimes lacking (e.g. for
Kreissparkasse connections)?
-}
localTransactions' :: ConnectionConfig -> Aq Text
localTransactions' conn =
  read conn aqbankingExe
    [ "export",
      "--ctxfile=" <> contextFile conn
    ]

{-|
Uses the @request@ subcommand to retrieve all transactions between the given
dates.
-}
getTransactions :: ConnectionConfig -> Day -> Day -> Aq ()
getTransactions conn from to =
  sequence_ . (<$> spans) $ \(from, to) ->
    run conn aqbankingExe
      [ "request",
        "--transactions",
        "--ignoreUnsupported",
        "--fromdate=" <> asAqDate from,
        "--todate=" <> asAqDate to,
        "--ctxfile=" <> contextFile conn
      ]
  where
    spans = spans' from
    spans' from'
      | diffDays to from' <= 30 = [(from, addDays 1 to)]
      | otherwise = (from', to') : spans' (addDays 1 to')
      where
        to' = addDays 30 from'
