{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- Module      : Text.Loquacious.Classes
   Description : Classes for Locales via Pretty Printer

Typically one would import Text.Loquacious, but importing this module
directly allows you to sidestep the default instances.
-}
module Text.Loquacious.Classes
  ( Loquacious(..)
  , SomeAnn(..)
  , Ann(..)
  , Doc
  ) where

import           Data.Text.Prettyprint.Doc (Doc)
import           Data.Typeable

-- | @SomeAnn@ encapsulates an annotation, allowing us to mix types of
-- annotations while building up a tree.
data SomeAnn = forall a. Ann a => SomeAnn a

-- | An Annotation Type Hierarchy, much like 'Exception' and 'SomeException'.
--
-- The general idea is that we can annotate the 'Doc' with any information we
-- want, and then the renderer will use 'alterAnnotationsS' to extract the sort
-- of annotations it cares about.
class Typeable a => Ann a where
  toAnn :: a -> SomeAnn
  toAnn = SomeAnn
  fromAnn :: SomeAnn -> Maybe a
  fromAnn (SomeAnn x) = cast x

instance Ann SomeAnn where
  toAnn a = a
  fromAnn = Just

-- | Runtime overridable version of the 'Pretty' class, parameterized by
-- locale and type.
class Typeable t => Loquacious l t where
  -- | The default pretty output.
  --
  -- For base types this can simply be 'pretty', perhaps with some
  -- annotation, but polymorphic types should implement this manually.
  loq :: t -> Doc SomeAnn
