{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- Module      : Text.Loquate.Instances
   Description : Default Loquate Instances
-}
module Text.Loquate.Instances
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
import           Data.Text.Prettyprint.Doc (Pretty(..))
import           Data.Typeable
import           Data.Version (Version, showVersion)
import           Data.Void (Void)
import           Data.Word (Word16, Word32, Word64, Word8)
import           GHC.Stack
import           Numeric.Natural
import           Text.Loquate.Class
import           Text.Loquate.Doc

-- misc types

instance Loquate l () where loq _ = pretty
instance Loquate l Bool where loq _ = pretty
instance Loquate l Version where loq _ = pretty . showVersion
instance Loquate l Void where loq _ = pretty

-- textual types

instance Loquate l Char where loq _ = pretty
instance {-# OVERLAPS #-} Loquate l String where loq _ = pretty
-- instance Loquate l BS.ByteString where loq _ = pretty
-- instance Loquate l LBS.ByteString where loq _ = pretty
instance Loquate l T.Text where loq _ = pretty
instance Loquate l LT.Text where loq _ = pretty

-- numeric types

instance Loquate l Double where loq _ = pretty
instance Loquate l Float where loq _ = pretty
instance (Typeable t, HasResolution t) => Loquate l (Fixed t) where loq _ = viaShow
-- | Integral types should loquate via Integer
instance Loquate l Integer where loq _ = pretty
instance Loquate l Integer => Loquate l Natural where loq l = loq l . toInteger

instance Loquate l Integer => Loquate l Int where loq l = loq l . toInteger
instance Loquate l Integer => Loquate l Int8 where loq l = loq l . toInteger
instance Loquate l Integer => Loquate l Int16 where loq l = loq l . toInteger
instance Loquate l Integer => Loquate l Int32 where loq l = loq l . toInteger
instance Loquate l Integer => Loquate l Int64 where loq l = loq l . toInteger

instance Loquate l Integer => Loquate l Word where loq l = loq l . toInteger
instance Loquate l Integer => Loquate l Word8 where loq l = loq l . toInteger
instance Loquate l Integer => Loquate l Word16 where loq l = loq l . toInteger
instance Loquate l Integer => Loquate l Word32 where loq l = loq l . toInteger
instance Loquate l Integer => Loquate l Word64 where loq l = loq l . toInteger

-- polymorphic numeric types

instance Loquate l t => Loquate l (Complex t) where
  loq l (a :+ b) = loq l a <+> "+" <+> loq l b <> "i"

instance Loquate l t => Loquate l (Ratio t) where
  loq l x = loq l (numerator x) <> "/" <> loq l (denominator x)

-- polymorphic containers in dependencies of loquacious

instance Loquate l t => Loquate l (Maybe t) where
  loq _ Nothing  = mempty
  loq l (Just x) = loq l x

instance (Loquate l t, Loquate l s) => Loquate l (t, s) where
  loq l (a, b) = tupled [loq l a, loq l b]

instance (Loquate l t, Loquate l s, Loquate l r) => Loquate l (t, s, r) where
  loq l (a, b, c) = tupled [loq l a, loq l b, loq l c]

instance (Loquate l t) => Loquate l [t] where
  loq l xs = list $ loq l <$> xs

instance (Loquate l t) => Loquate l (NonEmpty t) where
  loq l xs = list $ loq l <$> toList xs

instance (Typeable i, Loquate l t) => Loquate l (Array i t) where
  loq l xs = list $ loq l <$> toList xs

instance (Loquate l t, IArray UArray t, Ix i, Typeable i) => Loquate l (UArray i t) where
  loq l xs = list $ loq l <$> elems xs

instance (Loquate l t, Loquate l s) => Loquate l (Either t s) where
  loq l (Left x)  = loq l x
  loq l (Right x) = loq l x

-- showing types rather than values

instance Loquate l TypeRep where loq _ = viaShow

instance (Typeable a) => Loquate l (Proxy a) where
  loq l _ = loq l $ typeRep (Proxy :: Proxy (Proxy a))

instance (Typeable a, Typeable b) => Loquate l (a -> b) where
  loq l f = loq l $ typeOf f

instance (Typeable a, Typeable b) => Loquate l (a :~: b) where
  loq l _ = loq l (typeRep (Proxy :: Proxy a)) <+> ":~:" <+> loq l (typeRep (Proxy :: Proxy b))

instance (Typeable a, Typeable b) => Loquate l (a :~~: b) where
  loq l _ = loq l (typeRep (Proxy :: Proxy a)) <+> ":~~:" <+> loq l (typeRep (Proxy :: Proxy b))

-- call stacks!

instance Loquate l CallStack where
  loq l = withFrozenCallStack ( align . vsep . fmap loqStack . getCallStack )
    where loqStack (func, srcLoc) = fromString func <+> "←" <+> loq l srcLoc

instance Loquate l SrcLoc where
  loq l SrcLoc{..} = locFile <+> parens locPackage
    where
      locFile = fromString srcLocFile <> colon <> loq l srcLocStartLine <> colon <> loq l srcLocStartCol
      locPackage = fromString srcLocPackage <> colon <> fromString srcLocModule
