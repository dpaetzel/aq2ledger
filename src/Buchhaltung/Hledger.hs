{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Buchhaltung.Hledger
Description : Hledger-related helpers
Copyright   : David Pätzel, 2020
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

A collection of Hledger-related helper functions.
-}
module Buchhaltung.Hledger where

import Buchhaltung.Prelude
import Data.Time.Calendar (Day)
import Hledger.Data

{-|
Returns only the transactions from the list that were performed after the given
day (inclusively). If a second day is given, then only transactions up to this
day (inclusively) are returned.
-}
restrictTxs :: Day -> Maybe Day -> [Transaction] -> [Transaction]
restrictTxs from Nothing = filter (\tx -> from <= tdate tx)
restrictTxs from (Just to) = filter (\tx -> from <= tdate tx && tdate tx <= to)
