{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- Module      : Text.Loquacious.Instances
   Description : Default Loquacious Instances
-}
module Text.Loquacious.Instances
  (
  ) where

--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as LBS
import           Data.Array.Unboxed (Array, IArray, Ix, UArray, elems)
import           Data.Complex (Complex(..))
import           Data.Fixed
import           Data.Foldable
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Ratio (Ratio, denominator, numerator)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Prettyprint.Doc (Pretty(..), list, tupled, viaShow, (<+>))
import           Data.Typeable
import           Data.Version (Version, showVersion)
import           Data.Void (Void)
import           Data.Word (Word16, Word32, Word64, Word8)
import           Numeric.Natural
import           Text.Loquacious.Classes

-- misc types

instance Loquacious l () where loq = pretty
instance Loquacious l Bool where loq = pretty
instance Loquacious l Version where loq = pretty . showVersion
instance Loquacious l Void where loq = pretty

-- textual types

instance Loquacious l Char where loq = pretty
instance {-# OVERLAPS #-} Loquacious l String where loq = pretty
-- instance Loquacious l BS.ByteString where loq = pretty
-- instance Loquacious l LBS.ByteString where loq = pretty
instance Loquacious l T.Text where loq = pretty
instance Loquacious l LT.Text where loq = pretty

-- numeric types

instance Loquacious l Double where loq = pretty
instance Loquacious l Float where loq = pretty
instance (Typeable t, HasResolution t) => Loquacious l (Fixed t) where loq = viaShow
-- | Integral types should loquate via Integer
instance Loquacious l Integer where loq = pretty
instance Loquacious l Integer => Loquacious l Natural where loq = loq @l . toInteger

instance Loquacious l Integer => Loquacious l Int where loq = loq @l . toInteger
instance Loquacious l Integer => Loquacious l Int8 where loq = loq @l . toInteger
instance Loquacious l Integer => Loquacious l Int16 where loq = loq @l . toInteger
instance Loquacious l Integer => Loquacious l Int32 where loq = loq @l . toInteger
instance Loquacious l Integer => Loquacious l Int64 where loq = loq @l . toInteger

instance Loquacious l Integer => Loquacious l Word where loq = loq @l . toInteger
instance Loquacious l Integer => Loquacious l Word8 where loq = loq @l . toInteger
instance Loquacious l Integer => Loquacious l Word16 where loq = loq @l . toInteger
instance Loquacious l Integer => Loquacious l Word32 where loq = loq @l . toInteger
instance Loquacious l Integer => Loquacious l Word64 where loq = loq @l . toInteger

-- polymorphic numeric types

instance Loquacious l t => Loquacious l (Complex t) where
  loq (a :+ b) = loq @l a <+> "+" <+> loq @l b <> "i"

instance Loquacious l t => Loquacious l (Ratio t) where
  loq x = loq @l (numerator x) <> "/" <> loq @l (denominator x)

-- polymorphic containers in dependencies of loquacious

instance Loquacious l t => Loquacious l (Maybe t) where
  loq Nothing  = mempty
  loq (Just x) = loq @l x

instance (Loquacious l t, Loquacious l s) => Loquacious l (t, s) where
  loq (a, b) = tupled [loq @l a, loq @l b]

instance (Loquacious l t, Loquacious l s, Loquacious l r) => Loquacious l (t, s, r) where
  loq (a, b, c) = tupled [loq @l a, loq @l b, loq @l c]

instance (Loquacious l t) => Loquacious l [t] where
  loq xs = list $ loq @l <$> xs

instance (Loquacious l t) => Loquacious l (NonEmpty t) where
  loq xs = list $ loq @l <$> toList xs

instance (Typeable i, Loquacious l t) => Loquacious l (Array i t) where
  loq xs = list $ loq @l <$> toList xs

instance (Loquacious l t, IArray UArray t, Ix i, Typeable i) => Loquacious l (UArray i t) where
  loq xs = list $ loq @l <$> elems xs

instance (Loquacious l t, Loquacious l s) => Loquacious l (Either t s) where
  loq (Left x)  = loq @l x
  loq (Right x) = loq @l x

-- showing types rather than values

instance Loquacious l TypeRep where loq = viaShow
instance (Loquacious l a, Loquacious l b) => Loquacious l (a -> b) where
  loq f = loq @l $ typeOf f
instance (Loquacious l a, Loquacious l b) => Loquacious l (a :~: b) where
  loq _ = loq @l (typeRep (Proxy :: Proxy a)) <+> ":~:" <+> loq @l (typeRep (Proxy :: Proxy b))
instance (Loquacious l a, Loquacious l b) => Loquacious l (a :~~: b) where
  loq _ = loq @l (typeRep (Proxy :: Proxy a)) <+> ":~~:" <+> loq @l (typeRep (Proxy :: Proxy b))