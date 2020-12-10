{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-
A numeric data type rendered in multiple ways.
-}
module Data.Number where

import           Text.Loquacious

newtype Number = Number Rational
  deriving (Eq, Ord, Read, Show)

-- | The default instance just passes the buck to the wrapped type.
instance Loquacious l Number where
  loq (Number r) = loq @l r
