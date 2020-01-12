{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Buchhaltung.Format
Description : TODO
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

TODO
-}
module Buchhaltung.Format where

import Buchhaltung.Prelude hiding (many)
import Data.Decimal
import qualified Data.Text as T
import Data.Time.Format
import Hledger.Data
import Text.ParserCombinators.Parsec
import Prelude (read)

{-|
Note that we do not use the fields '$(localIban)' and '$(localAccountNumber)'
since they are not stable (aqbanking does usually only return one of the two
from a given bank; which one it is seems to be arbitrary?). Instead We require
this to be used in a context where the local account number or IBAN is already
known.
-}
-- "\$(dateAsString)#\$(valutaDateAsString)#\$(remoteIban)#\$(valueAsString)#\$(purposeInOneLine)"
listtransFormat :: String
listtransFormat =
  intercalate "#"
    [ "$(dateAsString)",
      "$(valutaDateAsString)",
      "$(remoteIban)",
      "$(remoteName)",
      "$(valueAsString)",
      "$(purposeInOneLine)"
    ]

exampleLine :: String
exampleLine =
  intercalate "#"
    [ dateAsString,
      valutaDateAsString,
      remoteIban,
      remoteName,
      valueAsString,
      purposeInOneLine
    ]
    ++ "\n"
  where
    dateAsString = "01.01.2000"
    valutaDateAsString = "02.01.2000"
    remoteIban = "DE1234567890123456"
    remoteName = "Max Mustermann"
    valueAsString = "1.23"
    purposeInOneLine = "Test"

{-|
Aqbanking's date format.
-}
dateFormat :: String
dateFormat = "%d.%m.%Y"

return []

runTests = $quickCheckAll
