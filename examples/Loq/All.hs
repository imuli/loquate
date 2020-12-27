{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-
Combined renderings.
-}
module Loq.All where

import           Loq.Decimal
import           Loq.Fract
import           Text.Loquate

-- | The top level list of possible locales to render into.
data NumLocale
   = Fract Fract
   | Dec Decimal

-- | given one of our languages, render some value.
loquate :: (Loquate Decimal t, Loquate Fract t) => NumLocale -> t -> Doq
loquate (Dec l)   = loq l
loquate (Fract l) = loq l
