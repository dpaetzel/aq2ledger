{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Buchhaltung.Aqbanking.Request
Description : TODO
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

TODO
-}
module Buchhaltung.Aqbanking.Request where

import Buchhaltung.Aqbanking
import Buchhaltung.Config
import Data.Text as T
import Protolude

{-|
From @aqbanking-cli listtrans --help@:

> Default is (all in one line):
>   $(dateOrValutaDateAsString)\t$(valueAsString)\t$(localBankcode)\t
>    $(localAccountNumber)\t$(localIban)\t$(remoteName)\t$(remoteIban)\t
>    $(purposeInOneLine)
-}
localTransactions :: Aq Text
localTransactions = do
  conf <- ask
  read aqbankingExe
    [ "listtrans",
      "--ctxfile=" <> contextFile conf,
      "--template=" <> "$(dateOrValutaDateAsString)\t$(localIban)\t$(remoteIban)\t$(valueAsString)\t$(purposeInOneLine)"
    ]

{-|
Uses the @export@ subcommand to list local transactions.

This seems to work not as stably as the @listtrans@ subcommand used by
'localTransaction' in that the account numbers are sometimes lacking (e.g. for
Kreissparkasse connections)?

  1="transactionId"
    2="localBankCode"
    3="localAccountNumber"
    4="remoteBankCode"
    5="remoteAccountNumber"
    6="date"
    7="valutadate"
    8="value/value"
    9="value/currency"
    10="localName"
    11="remoteName[0]"
    12="remoteName[1]"

-}
localTransactions' :: Aq Text
localTransactions' = do
  conf <- ask
  read aqbankingExe
    [ "export",
      "--ctxfile=" <> contextFile conf
    ]

-- TODO Find out whether dates are needed here
{-|
Uses the @request@ subcommand to retrieve transactions.
-}
getTransactions :: Aq ()
getTransactions = do
  conf <- ask
  run aqbankingExe
    [ "request",
      "--transactions",
      "--ignoreUnsupported",
      "-ctxfile=" <> contextFile conf
    ]
