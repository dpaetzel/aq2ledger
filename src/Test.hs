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

import Buchhaltung.Format as Format
import Buchhaltung.Parse as Parse
import Buchhaltung.Prelude

main :: IO ()
main = do
  _ <- Format.runTests
  _ <- Parse.runTests
  return ()
