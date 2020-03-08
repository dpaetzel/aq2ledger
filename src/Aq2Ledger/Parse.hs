{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Aq2Ledger.Parse
Description : Parsers from @aqbanking-cli@ to Hledger
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Parsers for converting @aqbanking-cli@ output to Haskell values (i.e. Hledger
'Hledger.Data.Transactions' etc.).
-}
module Aq2Ledger.Parse
  ( module Aq2Ledger.Parse.IBAN,
    AccountNameMap,
    listtrans,
    runTests,
  )
where

import Aq2Ledger.BankAccount
import Aq2Ledger.Format hiding (runTests)
import Aq2Ledger.Parse.IBAN
import Aq2Ledger.Prelude hiding (many)
import qualified Data.Text as T
import Data.Time.Format
import Hledger.Data
import Text.ParserCombinators.Parsec
import Prelude (read)

{-|
Maps bank accounts to account names to be used in Hledger journal.
-}
type AccountNameMap = BankAccount -> AccountName

{-|
Parses the output of @aqbanking-cli listtrans@ to a list of
'Hledger.Data.Transaction's.

The resulting value still requires an 'AccountNameMap' in order to properly
assign Hledger 'Hledger.Data.AccountName's to the bank accounts.

Note that the result is slightly opinionated as it uses my personal preference
of an 'Hledger.Data.AmountStyle'.
-}
listtrans
  :: IBANExtractor
  -> GenParser Char st (AccountNameMap -> [Transaction])
listtrans =
  fmap (\x accountNameMap -> fmap ($ accountNameMap) x) . many . line

{-|
Parses a single line of the output of @aqbanking-cli listtrans@.
-}
line :: IBANExtractor -> GenParser Char st (AccountNameMap -> Transaction)
line fromIBAN = do
  localAccountNumber' <- many (noneOf "#\n")
  char '#'
  localBankCode' <- many (noneOf "#\n")
  char '#'
  localIBAN <- many (noneOf "#\n")
  char '#'
  -- Sometimes we have account numbers, sometimes we only have IBANs, sometimes
  -- neither.
  let (localBankCode, localAccountNumber)
        | not . null $ localAccountNumber' = (localBankCode', localAccountNumber')
        | not . null $ localIBAN = fromIBAN localIBAN
        | otherwise = ("", "")
  date <-
    parseTimeOrError True defaultTimeLocale outDateFormat <$> many (noneOf "#\n")
  char '#'
  valutaDate <-
    parseTimeOrError True defaultTimeLocale outDateFormat <$> many (noneOf "#\n")
  char '#'
  remoteIBAN <- many (noneOf "#\n")
  char '#'
  remoteName <- many (noneOf "#\n")
  char '#'
  -- NOTE There has to be a nicer way to parse to Decimals.
  sign' <- optionMaybe (char '-')
  let sign =
        case sign' of
          Just _ -> negate
          _ -> identity
  digits1 <- many digit
  char '.'
  digits2 <- many digit
  let mantissa = fromIntegral . length $ digits2
  let value = sign $ Decimal mantissa (read digits1 * 10 ^ mantissa + read digits2)
  char '#'
  purpose <- many (noneOf "\n")
  char '\n'
  return $ \mapping ->
    nulltransaction
      { tcomment =
          "\n"
            <> "Generated by aq2ledger\n"
            <> "Local bank code: "
            <> T.pack localBankCode
            <> "\n"
            <> "Local account number: "
            <> T.pack localAccountNumber
            <> "\n"
            <> "Remote IBAN: "
            <> T.pack remoteIBAN
            <> "\n"
            <> "Remote Name: "
            <> T.pack remoteName
            <> "\n",
        tdate = date,
        tdate2 = Just valutaDate,
        tdescription = T.pack purpose,
        tpostings =
          [ post (mapping (BankAccount localBankCode localAccountNumber))
              $ (eur value) {acommodity = "EUR", astyle = amountStyle},
            post "TODO"
              $ (eur $ negate value) {acommodity = "EUR", astyle = amountStyle}
          ]
      }

{-|
The amount style I personally prefer.
-}
amountStyle :: AmountStyle
amountStyle =
  AmountStyle
    { ascommodityside = R,
      ascommodityspaced = True,
      asprecision = 2,
      asdecimalpoint = Just ',',
      asdigitgroups = Nothing
    }

prop_exampleLineParses =
  isRight . parse (line fromIBANDE) "" $ exampleListtransLine

return []

runTests = $quickCheckAll
