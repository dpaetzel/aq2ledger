{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Aq2Ledger
Description : TODO
Copyright   : David Pätzel, 2020
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

TODO
-}
module Aq2Ledger where

import Aq2Ledger.Prelude
import Hledger.Data

{-|
Returns a list of transactions and a list of candidates for being duplicates
(the first one being the new entry---to be inserted if it is not a duplicate,
the second one being the old entry which is already part of the returned list).
-}
merge
  :: [Transaction]
  -> [Transaction]
  -> ([Transaction], [(Transaction, [Transaction])])
merge txOlds txNews =
  foldl f (txOlds, []) txNews
  where
    f r@(txOlds, txDups) tx
      | tx `elem` txOlds = r
      | tx `notElem` txOlds =
        let dups = possibleDuplicates tx (txOlds <> (fst <$> txDups))
         in if null dups
              then (tx : txOlds, txDups)
              else (txOlds, (tx, dups) : txDups)

-- TODO Just uncomment this
-- prop_merge_oldEmptyNoDuplicates txNews =
--   merge [] (nub txNews) == (nub txNews, [])

-- TODO Test this merge
-- TODO Add earliest date to download to reduce number of possibleDuplicates during adoption phase
possibleDuplicates :: Transaction -> [Transaction] -> [Transaction]
possibleDuplicates tx = filter (\tx' -> tx /= tx' && isPossibleDuplicate tx tx')

isPossibleDuplicate :: Transaction -> Transaction -> Bool
isPossibleDuplicate tx1 tx2 = key tx1 == key tx2

-- TODO Should the IBAN/account number be added here (e.g. by extracting it from
-- a comment)?
key tx =
  ( tdate tx,
    sort . positive . tpostings $ tx
  )

{-|
Sums of the positive quantities in the given postings. One sum for each
different commodity, the result is sorted ascending.
-}
positive :: [Posting] -> [Decimal]
positive =
  sort
    . fmap (sum . fmap aquantity)
    . groupOn acommodity
    . filter ((0 <) . aquantity)
    . mconcat
    . fmap (amounts . pamount)

return []

runTests = $quickCheckAll
