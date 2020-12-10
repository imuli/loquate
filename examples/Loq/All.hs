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
import           Text.Loquacious

-- | The top level list of possible locales to render into.
data NumLocale
   = Mixed
   | Improper
   | Decimal

-- | given one of our languages, render some value.
loquate :: (Loquacious Improper t, Loquacious Mixed t, Loquacious Decimal t) =>
  NumLocale -> t -> Doc SomeAnn
loquate Decimal  = loq @Decimal
loquate Improper = loq @Improper
loquate Mixed    = loq @Mixed
