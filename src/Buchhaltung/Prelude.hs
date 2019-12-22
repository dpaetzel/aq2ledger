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
  ( module Protolude,
    module Buchhaltung.Prelude,
  )
where

import Protolude

{-|
We need this type alias because 'System.Process' uses 'String's.
-}
type String = [Char]
