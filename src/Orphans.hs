{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans (
  ) where

import           Data.Diff
import qualified Generics.SOP           as SOP
import qualified Text.Pandoc.Definition as P

instance SOP.Generic P.Pandoc
instance SOP.Generic P.Meta
instance SOP.Generic P.MetaValue
instance SOP.Generic P.Block
instance SOP.Generic P.Inline
instance SOP.Generic P.Format
instance SOP.Generic P.QuoteType
instance SOP.Generic P.ListNumberStyle
instance SOP.Generic P.ListNumberDelim
instance SOP.Generic P.Alignment
instance SOP.Generic P.MathType
instance SOP.Generic P.Citation
instance SOP.Generic P.CitationMode

instance SOP.HasDatatypeInfo P.Pandoc
instance SOP.HasDatatypeInfo P.Meta
instance SOP.HasDatatypeInfo P.MetaValue
instance SOP.HasDatatypeInfo P.Block
instance SOP.HasDatatypeInfo P.Inline
instance SOP.HasDatatypeInfo P.Format
instance SOP.HasDatatypeInfo P.QuoteType
instance SOP.HasDatatypeInfo P.ListNumberStyle
instance SOP.HasDatatypeInfo P.ListNumberDelim
instance SOP.HasDatatypeInfo P.Alignment
instance SOP.HasDatatypeInfo P.MathType
instance SOP.HasDatatypeInfo P.Citation
instance SOP.HasDatatypeInfo P.CitationMode

instance Diff P.Pandoc
instance Diff P.Meta
instance Diff P.MetaValue
instance Diff P.Block
instance Diff P.Inline
instance Diff P.Format
instance Diff P.QuoteType
instance Diff P.ListNumberStyle
instance Diff P.ListNumberDelim
instance Diff P.Alignment
instance Diff P.MathType
instance Diff P.Citation
instance Diff P.CitationMode
