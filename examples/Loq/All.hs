{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Combined renderings.
-}
module Loq.All where

import           Data.Text.Loquacious
import           Loq.Decimal
import           Loq.Fract

-- | The top level list of possible locales to render into.
data NumberTypes
   = Fract Fract
   | Deci Decimal

-- | given one of our languages, render some value.
loquate :: (Loquacious Fract t, Loquacious Decimal t) =>
  NumberTypes -> t -> Doc SomeAnn
loquate (Fract l) = loq l
loquate (Deci l)  = loq l
