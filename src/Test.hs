{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Test
Description : Test suite
Copyright   : David Pätzel, 2020
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Test suite accumulating all tests in the project.
-}
module Main where

import Aq2Ledger
import Aq2Ledger.Format as Format
import Aq2Ledger.Parse as Parse
import Aq2Ledger.Prelude

main :: IO ()
main = do
  _ <- Format.runTests
  _ <- Aq2Ledger.runTests
  _ <- Parse.runTests
  return ()
