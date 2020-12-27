{- Module      : Text.Loquate.Doc
   Description : Document Creation

This is a partial reexport of Data.Text.Prettyprint.Doc, with only those
functions related to document creation, and some extra helper functions.
-}
module Text.Loquate.Doc
  ( -- * Document
    Doc
    -- * Basics
  , fromString
  , viaShowS
  , viaShow
  , unsafeViaShow
  , emptyDoc
    -- ** Line breaks
  , line
  , line'
  , softline
  , softline'
  , hardline
    -- ** Primitives for alternative layouts
  , group
  , flatAlt
    -- * Alignment functions
  , nest
  , align
  , hang
  , indent
  , encloseSep
  , list
  , tupled
    -- * Joining functions
    -- ** Binary
  , (<>)
  , (<+>)
    -- ** List
  , concatWith
    -- *** 'sep' family
    --
    -- | When 'group'ed, these will replace newlines with spaces.
  , hsep
  , vsep
  , fillSep
  , sep
    -- *** 'cat' family
    --
    -- | When 'group'ed, these will remove newlines.
  , hcat
  , vcat
  , fillCat
  , cat
    -- *** Others
  , punctuate
    -- * Reactive/conditional layouts
    --
    -- | Lay documents out differently based on current position and the page
    -- layout.
  , column
  , nesting
  , width
  , pageWidth
    -- * Filler functions
    --
    -- | Fill up available space
  , fill
  , fillBreak
    -- * General convenience
    --
    -- | Useful helper functions.
  , plural
  , enclose
  , surround
    -- * Bracketing functions
    --
    -- | Enclose documents in common ways.
  , squotes
  , dquotes
  , parens
  , angles
  , brackets
  , braces
    -- * Named characters
    --
    -- | Convenience definitions for common characters
  , lparen
  , rparen
  , langle
  , rangle
  , lbrace
  , rbrace
  , lbracket
  , rbracket
  , squote
  , dquote
  , semi
  , colon
  , comma
  , space
  , dot
  , slash
  , backslash
  , equals
  , pipe
    -- * Optimization
  , fuse
  , FusionDepth(..)
  ) where

import           Data.String
import           Data.Text.Prettyprint.Doc
import           Text.Loquate.Helpers
