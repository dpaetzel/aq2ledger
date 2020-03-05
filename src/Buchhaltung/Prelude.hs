{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Buchhaltung.Prelude
Description : Custom prelude based on Protolude
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

Custom prelude based on 'Protolude' with a few additions.
-}
module Buchhaltung.Prelude
  ( module Buchhaltung.Prelude,
    module X,
  )
where

import Data.Decimal as X
import Data.List.Extra as X (groupOn)
import Protolude as X
import Test.QuickCheck as X hiding
  ( (.&.),
  )
import qualified Prelude

{-|
This type alias is needed because 'System.Process' uses 'Prelude.String's.
-}
type String = Prelude.String
