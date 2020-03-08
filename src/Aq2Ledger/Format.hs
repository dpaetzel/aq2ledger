{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Aq2Ledger.Format
Description : Format strings
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

The format strings used for calls of @aqbanking-cli@.
-}
module Aq2Ledger.Format where

import Aq2Ledger.Prelude hiding (many)
import Data.Time (Day, defaultTimeLocale, formatTime)

{-|
We do use both '$(localIBAN)' as well as '$(localBankCode)' and
'$(localAccountNumber)' since neither of those are stable (@aqbanking-cli
listtrans@ does usually only return one of the two; which one seems to be
arbitrary and dependent on the bank?).
-}
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

{-|
An example line returned by @aqbanking listtrans --template=F@ where @F =
'listtransFormat'@.
-}
exampleListtransLine :: String
exampleListtransLine =
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
Output date format used by @aqbanking-cli@.
-}
outDateFormat :: String
outDateFormat = "%d.%m.%Y"

{-|
Input date format used by @aqbanking-cli@.
-}
inDateFormat :: String
inDateFormat = "%Y%m%d"

{-|
Formats a 'Data.Time.Calendar.Day' as a AqBanking-compatible string.
-}
asAqDate :: Day -> String
asAqDate = formatTime defaultTimeLocale inDateFormat

return []

runTests = $quickCheckAll
