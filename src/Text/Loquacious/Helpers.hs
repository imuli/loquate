module Text.Loquacious.Helpers
  ( viaShowS
  ) where

import           Data.Text.Prettyprint.Doc (Pretty(..))
import           Text.Loquacious.Classes

viaShowS :: (t -> ShowS) -> t -> Doc a
viaShowS f = pretty . flip f ""
