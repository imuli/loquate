{- Module      : Data.Text.Loquacious
   Description : Locale Sensitive Pretty Printer
-}
module Data.Text.Loquacious
  ( Loquacious(..)
  , SomeAnn(..)
  , Ann(..)
  , Raw(..)
  , Doc
  ) where

import           Data.Text.Loquacious.Classes
import           Data.Text.Loquacious.Instances ()
import           Data.Text.Loquacious.Raw
