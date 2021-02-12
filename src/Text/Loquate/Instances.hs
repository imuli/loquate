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
import qualified Control.Exception.Base as E
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

instance Lang l => Loquate l () where loq _ = pretty
instance Lang l => Loquate l Bool where loq _ = pretty
instance Lang l => Loquate l Version where loq _ = pretty . showVersion
instance Lang l => Loquate l Void where loq _ = pretty

-- textual types

instance Lang l => Loquate l Char where loq _ = pretty
instance {-# OVERLAPS #-} Lang l => Loquate l String where loq _ = pretty
-- instance Loquate l BS.ByteString where loq _ = pretty
-- instance Loquate l LBS.ByteString where loq _ = pretty
instance Lang l => Loquate l T.Text where loq _ = pretty
instance Lang l => Loquate l LT.Text where loq _ = pretty

-- numeric types

instance Lang l => Loquate l Double where loq _ = pretty
instance Lang l => Loquate l Float where loq _ = pretty
instance (Lang l, Typeable t, HasResolution t) => Loquate l (Fixed t) where loq _ = viaShow
-- | Integral types should loquate via Integer
instance Lang l => Loquate l Integer where loq _ = pretty
instance (Lang l, Loquate l Integer) => Loquate l Natural where loq l = loq l . toInteger

instance (Lang l, Loquate l Integer) => Loquate l Int where loq l = loq l . toInteger
instance (Lang l, Loquate l Integer) => Loquate l Int8 where loq l = loq l . toInteger
instance (Lang l, Loquate l Integer) => Loquate l Int16 where loq l = loq l . toInteger
instance (Lang l, Loquate l Integer) => Loquate l Int32 where loq l = loq l . toInteger
instance (Lang l, Loquate l Integer) => Loquate l Int64 where loq l = loq l . toInteger

instance (Lang l, Loquate l Integer) => Loquate l Word where loq l = loq l . toInteger
instance (Lang l, Loquate l Integer) => Loquate l Word8 where loq l = loq l . toInteger
instance (Lang l, Loquate l Integer) => Loquate l Word16 where loq l = loq l . toInteger
instance (Lang l, Loquate l Integer) => Loquate l Word32 where loq l = loq l . toInteger
instance (Lang l, Loquate l Integer) => Loquate l Word64 where loq l = loq l . toInteger

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
  loq l (a, b) = align $ tupled [loq l a, loq l b]

instance (Loquate l t, Loquate l s, Loquate l r) => Loquate l (t, s, r) where
  loq l (a, b, c) = align $ tupled [loq l a, loq l b, loq l c]

instance (Loquate l t) => Loquate l [t] where
  loq l xs = align . list $ loq l <$> xs

instance (Loquate l t) => Loquate l (NonEmpty t) where
  loq l xs = align . list $ loq l <$> toList xs

instance (Typeable i, Loquate l t) => Loquate l (Array i t) where
  loq l xs = align . list $ loq l <$> toList xs

instance (Loquate l t, IArray UArray t, Ix i, Typeable i) => Loquate l (UArray i t) where
  loq l xs = align . list $ loq l <$> elems xs

instance (Loquate l t, Loquate l s) => Loquate l (Either t s) where
  loq l (Left x)  = loq l x
  loq l (Right x) = loq l x

-- showing types rather than values

instance Lang l => Loquate l TypeRep where loq _ = viaShow

instance (Lang l, Typeable a) => Loquate l (Proxy a) where
  loq l _ = loq l $ typeRep (Proxy :: Proxy (Proxy a))

instance (Lang l, Typeable a, Typeable b) => Loquate l (a -> b) where
  loq l f = loq l $ typeOf f

instance (Lang l, Typeable a, Typeable b) => Loquate l (a :~: b) where
  loq l _ = loq l (typeRep (Proxy :: Proxy a)) <+> ":~:" <+> loq l (typeRep (Proxy :: Proxy b))

instance (Lang l, Typeable a, Typeable b) => Loquate l (a :~~: b) where
  loq l _ = loq l (typeRep (Proxy :: Proxy a)) <+> ":~~:" <+> loq l (typeRep (Proxy :: Proxy b))

-- call stacks!

instance Lang l => Loquate l CallStack where
  loq l = withFrozenCallStack ( align . vsep . fmap loqStack . getCallStack )
    where loqStack (func, srcLoc) = fromString func <+> "←" <+> loq l srcLoc

instance Lang l => Loquate l SrcLoc where
  loq l SrcLoc{..} = locFile <+> parens locPackage
    where
      locFile = fromString srcLocFile <> colon <> loq l srcLocStartLine <> colon <> loq l srcLocStartCol
      locPackage = fromString srcLocPackage <> colon <> fromString srcLocModule

-- Exception types

instance Lang l => Loquate l E.AllocationLimitExceeded where loq _ e = viaShow e
instance Lang l => Loquate l E.ArithException where loq _ e = viaShow e
instance Lang l => Loquate l E.ArrayException where loq _ e = viaShow e
instance Lang l => Loquate l E.AssertionFailed where loq _ e = viaShow e
instance Lang l => Loquate l E.AsyncException where loq _ e = viaShow e
instance Lang l => Loquate l E.BlockedIndefinitelyOnMVar where loq _ e = viaShow e
instance Lang l => Loquate l E.BlockedIndefinitelyOnSTM where loq _ e = viaShow e
instance Lang l => Loquate l E.CompactionFailed where loq _ e = viaShow e
instance Lang l => Loquate l E.Deadlock where loq _ e = viaShow e
instance Lang l => Loquate l E.FixIOException where loq _ e = viaShow e
instance Lang l => Loquate l E.IOException where loq _ e = viaShow e
instance Lang l => Loquate l E.NestedAtomically where loq _ e = viaShow e
instance Lang l => Loquate l E.NoMethodError where loq _ e = viaShow e
instance Lang l => Loquate l E.NonTermination where loq _ e = viaShow e
instance Lang l => Loquate l E.PatternMatchFail where loq _ e = viaShow e
instance Lang l => Loquate l E.RecConError where loq _ e = viaShow e
instance Lang l => Loquate l E.RecSelError where loq _ e = viaShow e
instance Lang l => Loquate l E.RecUpdError where loq _ e = viaShow e
instance Lang l => Loquate l E.SomeAsyncException where loq _ e = viaShow e
instance Lang l => Loquate l E.SomeException where loq _ e = viaShow e
instance Lang l => Loquate l E.TypeError where loq _ e = viaShow e
