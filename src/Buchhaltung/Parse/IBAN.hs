{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Buchhaltung.Parse.IBAN
Description : TODO
Copyright   : David Pätzel, 2020
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

TODO
-}
module Buchhaltung.Parse.IBAN where

import Buchhaltung.Prelude

{-|
A function that extracts bank code and account number from an IBAN.
-}
type IBANExtractor = String -> (String, String)

{-|
Extracts bank code and account number from a German IBAN without spaces.
-}
fromIBANDE :: IBANExtractor
fromIBANDE iban =
  let bankCode = take 8 . drop 4 $ iban
      accountNumber = dropWhile (== '0') . drop (8 + 4) $ iban
   in (bankCode, accountNumber)