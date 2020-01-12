{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Buchhaltung.Prelude
Description : TODO
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

TODO
-}
module Buchhaltung.Prelude
  ( module Buchhaltung.Prelude,
    module X,
  )
where

import Protolude as X
import Test.QuickCheck as X hiding
  ( (.&.),
  )

{-|
We need this type alias because 'System.Process' uses 'String's.
-}
type String = [Char]
