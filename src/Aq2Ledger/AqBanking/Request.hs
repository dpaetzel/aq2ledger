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
import System.Directory (doesFileExist)

{-|
Uses the @listtrans@ subcommand to list all local transactions for the
'ConnectionConfig' currently selected.

If the connection's CTX file does not exist, returns an empty 'Data.Text.Text'
object instead of throwing the corresponding error (simply because that seems to
be sensible).
-}
localTransactions :: ConnectionName -> Aq Text
localTransactions nam = do
  con <- connection nam
  exists <- liftIO . doesFileExist $ contextFile con
  if exists
    then
      read con aqbankingExe
        [ "listtrans",
          "--ctxfile=" <> contextFile con,
          "--template=" <> listtransFormat
        ]
    else return mempty

{-|
Uses the @export@ subcommand to list local transactions.

This seems to work not as stably as the @listtrans@ subcommand used by
'localTransactions' in that the account numbers are sometimes lacking (e.g. for
Kreissparkasse connections)?

If the connection's CTX file does not exist, returns an empty 'Data.Text.Text'
object instead of throwing the corresponding error (simply because that seems to
be sensible).
-}
localTransactions' :: ConnectionName -> Aq Text
localTransactions' nam = do
  con <- connection nam
  exists <- liftIO . doesFileExist $ contextFile con
  if exists
    then
      read con aqbankingExe
        [ "export",
          "--ctxfile=" <> contextFile con
        ]
    else return mempty

{-|
Uses the @request@ subcommand to retrieve all transactions between the given
dates.
-}
getTransactions :: ConnectionName -> Day -> Day -> Aq ()
getTransactions nam from to = do
  con <- connection nam
  putInfo
    $ "Downloading all transactions between "
      <> show from
      <> " and "
      <> show to
      <> " for connection '"
      <> name con
      <> "'."
  interact con aqbankingExe
    [ "request",
      "--transactions",
      "--ignoreUnsupported",
      "--fromdate=" <> asAqDate from,
      "--todate=" <> asAqDate to,
      "--ctxfile=" <> contextFile con
    ]
