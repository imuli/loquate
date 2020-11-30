{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{- Module      : Data.Text.Loquacious.Raw
   Description : Loquacious Instances that look a lot like 'Show'.
-}
module Data.Text.Loquacious.Raw
  ( Raw
  , rawRadix
  , Radix(..)
  ) where

import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Text.Loquacious.Classes
import           Data.Text.Loquacious.Helpers
import           Data.Text.Loquacious.Instances ()
import           Data.Text.Prettyprint.Doc (Pretty(..), (<+>))
import           Numeric

-- | What Radix to use when 'loq'uating numbers.
data Radix
   = Octal
   | Decimal
   | Hexdecimal
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Show-like Locales
data Raw = Raw
   { rawRadix :: Radix
   }
  deriving (Eq, Ord, Read, Show)

digits :: Int -> Char
digits n = fromMaybe 'x' $ listToMaybe $ drop n "0123456789abcdef"

instance {-# OVERLAPS #-} Loquacious Raw Integer where
  loq (Raw Octal)      = viaShowS $ showSigned (\n' -> ("0o" <>) . showIntAtBase 8 digits n') 0
  loq (Raw Decimal)    = pretty
  loq (Raw Hexdecimal) = viaShowS $ showSigned (\n' -> ("0x" <>) . showHex n') 0

instance {-# OVERLAPS #-} Loquacious Raw Double where
  loq (Raw Octal)      = pretty -- FIXME
  loq (Raw Decimal)    = pretty
  loq (Raw Hexdecimal) = viaShowS showHFloat

instance {-# OVERLAPS #-} Loquacious Raw Float where
  loq (Raw Octal)      = pretty -- FIXME
  loq (Raw Decimal)    = pretty
  loq (Raw Hexdecimal) = viaShowS showHFloat

instance {-# OVERLAPS #-} Loquacious Raw a => Loquacious Raw (Maybe a) where
  loq _ Nothing  = "Nothing"
  loq l (Just x) = "Just" <+> loq l x

instance {-# OVERLAPS #-} (Loquacious Raw t, Loquacious Raw s) => Loquacious Raw (Either t s) where
  loq l (Left x)  = "Left" <+> (loq l x)
  loq l (Right x) = "Right" <+> (loq l x)
