{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-
Decimal renderings.
-}
module Loq.Decimal where

import           Data.Number
import           Text.Loquate

data Decimal
   = Decimal

-- | convert to a double and print that
instance {-# OVERLAPS #-} Loquate Decimal Number where
  loq Decimal (Number r) =
    let r' = fromRational r :: Double
     in loq () r'
