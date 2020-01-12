{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Buchhaltung.Parse
Description : TODO
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

TODO
-}
module Buchhaltung.Parse where

import Buchhaltung.Format
import Buchhaltung.Prelude hiding (many)
import Data.Decimal
import qualified Data.Text as T
import Data.Time.Format
import Hledger.Data
import Text.ParserCombinators.Parsec
import Prelude (read)

listtrans :: GenParser Char st [AccountName -> Transaction]
listtrans = many line

line :: GenParser Char st (AccountName -> Transaction)
line = do
  date <-
    parseTimeOrError True defaultTimeLocale dateFormat <$> many (noneOf "#\n")
  char '#'
  valutaDate <-
    parseTimeOrError True defaultTimeLocale dateFormat <$> many (noneOf "#\n")
  char '#'
  remoteIBAN <- many (noneOf "#\n")
  char '#'
  remoteName <- many (noneOf "#\n")
  char '#'
  -- NOTE There must be a nicer way to parse to Decimals
  sign <- optionMaybe (char '-')
  digits1 <- many digit
  char '.'
  digits2 <- many digit
  let mantissa = fromIntegral $ length digits2
  let value = Decimal mantissa (read digits1 * 10 ^ mantissa + read digits2)
  char '#'
  purpose <- many (noneOf "\n")
  char '\n'
  return $ \account ->
    nulltransaction
      { tcomment =
          "\n"
            <> "Generated by buchhaltung2\n"
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
          [ post account $ (eur value) {acommodity = "EUR", astyle = amountStyle},
            post "TODO" $ (eur $ negate value) {acommodity = "EUR", astyle = amountStyle}
          ]
      }

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
  isRight . parse line "" $ exampleLine

return []

runTests = $quickCheckAll
