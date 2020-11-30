{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Decimal renderings.
-}
module Loq.Decimal where

import           Data.Number
import           Data.Text.Loquacious

data Decimal
   = Decimal

-- | convert to a double and print that
instance {-# OVERLAPS #-} Loquacious Decimal Number where
  loq Decimal (Number r) =
    let r' = fromRational r :: Double
     in loq () r'
