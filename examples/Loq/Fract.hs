{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-
Fractional renderings.
-}
module Loq.Fract where

import           Data.Number
import           Data.Ratio
import           Data.Text.Loquacious

data Mixed
data Improper

instance {-# OVERLAPS #-} Loquacious Mixed Number where
  loq (Number r) =
    let neg_ = if signum r < 0 then "-" else mempty
        r' = abs r
        numer = numerator r'
        denom = denominator r'
        lead = numer `div` denom
        numer' = numer - lead * denom
        lead_ = if lead /= 0 || numer' == 0 then loq @() lead else mempty
        sep_ = if lead /= 0 && numer' /= 0 then "-" else mempty
        frac_ = if numer' /= 0 then loq @() numer' <> "/" <> loq @() denom else mempty
     in neg_ <> lead_ <> sep_ <> frac_
instance {-# OVERLAPS #-} Loquacious Improper Number where
  loq (Number r) =
    let neg = if signum r < 0 then "-" else mempty
        r' = abs r
        numer = numerator r'
        denom = denominator r'
     in neg <> loq @() numer <> "/" <> loq @() denom
