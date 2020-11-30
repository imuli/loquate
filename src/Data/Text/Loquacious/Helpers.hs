module Data.Text.Loquacious.Helpers
  ( viaShowS
  ) where

import           Data.Text.Loquacious.Classes
import           Data.Text.Prettyprint.Doc (Pretty(..))

viaShowS :: (t -> ShowS) -> t -> Doc a
viaShowS f = pretty . flip f ""
