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

localTransactions :: Aq Text
localTransactions = do
  conf <- ask
  read aqbankingExe
    [ "export",
      "-c",
      contextFile conf
    ]

getTransactions :: Aq ()
getTransactions = do
  conf <- ask
  run aqbankingExe
    [ "request",
      "--transactions",
      "--ignoreUnsupported",
      "-c",
      contextFile conf
    ]
