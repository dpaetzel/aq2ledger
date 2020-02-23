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

-- Note that we do not use the fields '$(localIban)' and '$(localAccountNumber)'
-- since they are not stable (aqbanking does usually only return one of the two
-- from a given bank; which one it is seems to be arbitrary?). Instead we require
-- this to be used in a context where the local account number or IBAN is already
-- known.
--
-- There is a big problem with this approach, namely that some transactions
-- don't have any indicator to which account they belong (at least for
-- Kreissparkasse) and thus vanish if we use `listtrans --account=…` for every
-- account there is although they do technically have to belong to one of the
-- accounts.
-- "\$(dateAsString)#\$(valutaDateAsString)#\$(remoteIban)#\$(valueAsString)#\$(purposeInOneLine)"
listtransFormat :: String
listtransFormat =
  intercalate "#"
    [ "$(localAccountNumber)",
      "$(localBankCode)",
      "$(localIBAN)",
      "$(dateAsString)",
      "$(valutaDateAsString)",
      "$(remoteIban)",
      "$(remoteName)",
      "$(valueAsString)",
      "$(purposeInOneLine)"
    ]

exampleLine :: String
exampleLine =
  intercalate "#"
    [ localAccountNumber,
      localBankCode,
      localIBAN,
      dateAsString,
      valutaDateAsString,
      remoteIban,
      remoteName,
      valueAsString,
      purposeInOneLine
    ]
    ++ "\n"
  where
    localAccountNumber = ""
    localBankCode = ""
    localIBAN = "DE12345678900000000000"
    dateAsString = "01.01.2000"
    valutaDateAsString = "02.01.2000"
    remoteIban = "DE1234567890123456"
    remoteName = "Max Mustermann"
    valueAsString = "1.23"
    purposeInOneLine = "Test"

{-|
Aqbanking's output date format.
-}
dateFormat :: String
dateFormat = "%d.%m.%Y"

return []

runTests = $quickCheckAll
