module Text.Loquate.Helpers
  ( viaShowS
  ) where

import           Data.Text.Prettyprint.Doc (Doc, Pretty(..))

viaShowS :: (t -> ShowS) -> t -> Doc a
viaShowS f = pretty . flip f ""
