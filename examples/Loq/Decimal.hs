{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-
Decimal renderings.
-}
module Loq.Decimal where

import           Data.Number
import           Text.Loquacious

data Decimal

-- | convert to a double and print that
instance {-# OVERLAPS #-} Loquacious Decimal Number where
  loq (Number r) =
    let r' = fromRational r :: Double
     in loq @() r'
