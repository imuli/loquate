{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- Module      : Text.Loquate.Classes
   Description : Classes for Locales via Pretty Printer

Typically one would import Text.Loquate, but importing this module
directly allows you to sidestep the default instances.
-}
module Text.Loquate.Class
  ( Loquate(..)
  , Annotation(..)
  , Ann(..)
  , Doq
  ) where

import           Data.Text.Prettyprint.Doc (Doc)
import           Data.Typeable

-- | Encapsulate an annotation, allowing us to mix types of annotations while
-- building up a tree.
data Ann = forall a. Annotation a => Ann a

-- | An Annotation Type Hierarchy, much like 'Exception' and 'SomeException'.
--
-- The general idea is that we can annotate the 'Doc' with any information we
-- want, and then the renderer will use 'alterAnnotationsS' to extract the sort
-- of annotations it cares about.
class Typeable a => Annotation a where
  toAnn :: a -> Ann
  toAnn = Ann
  fromAnn :: Ann -> Maybe a
  fromAnn (Ann x) = cast x

instance Annotation Ann where
  toAnn a = a
  fromAnn = Just

-- | Annotation-Generic 'Doc'uments
type Doq = Doc Ann

-- | Runtime-overridable version of the 'Pretty' class, parameterized by
-- locale and type.
class Typeable t => Loquate l t where
  -- | Pretty output for a set of languages.
  --
  -- For base types this can simply be 'pretty', perhaps with some
  -- annotation, but polymorphic types should implement this manually.
  loq :: l -> t -> Doq
