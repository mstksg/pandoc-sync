{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeInType         #-}

module Text.Pandoc.Sync.Format (
    MarkdownType(..)
  , SlideShowType(..)
  , Format(..)
  , WriterFormat(..)
  , ReaderOptions(..)
  , _ROReadable
  , _ROUnreadable
  , formatReader
  , formatWriter
  , readerString
  , writerString
  ) where

import           Control.Lens
import           Data.Kind
import           Data.Maybe
import           Data.Singletons
import qualified Text.Pandoc     as P

data MarkdownType = MDPandoc
                  | MDStrict
                  | MDPHP
                  | MDGithub
                  | MDMulti
                  | MDCommon
  deriving Show

data SlideShowType = SSS5
                   | SSSlidy
                   | SSSlideous
                   | SSDZSlides
                   | SSRevealJS
                   | SSBeamer
  deriving Show

data Format :: Bool -> Bool -> Type where
    FNative       :: Format 'True  'True
    FJSON         :: Format 'True  'True
    FMarkdown     :: MarkdownType
                  -> Format 'True  'True
    FRST          :: Format 'True  'True
    FMediaWiki    :: Format 'True  'True
    FDocBook      :: Format 'True  'True
    FOPML         :: Format 'True  'True
    FOrg          :: Format 'True  'True
    FTextile      :: Format 'True  'True
    FHTML         :: Bool
                  -> Format 'True  'True
    FLaTeX        :: Format 'True  'True
    FHaddock      :: Format 'True  'True
    FTWiki        :: Format 'True  'False
    FDocX         :: Format 'True  'True
    FODT          :: Format 'True  'True
    FT2T          :: Format 'True  'False
    FEPub         :: Bool
                  -> Format 'True  'True
    FFictionBook2 :: Format 'False 'True
    FICML         :: Format 'False 'True
    FSlideShow    :: SlideShowType
                  -> Format 'False 'True
    FOpenDocument :: Format 'False 'True
    FContext      :: Format 'False 'True
    FTexinfo      :: Format 'False 'True
    FMan          :: Format 'False 'True
    FPlain        :: Format 'False 'True
    FDokuWiki     :: Format 'False 'True
    FASCIIDoc     :: Format 'False 'True
    FTEI          :: Format 'False 'True

deriving instance Show (Format r w)

-- data ReaderFormat :: Type where
--     ReaderFormat :: SingI w => Format 'True w -> ReaderFormat

data WriterFormat :: Type where
    WriterFormat :: SingI r => Format r 'True -> WriterFormat

-- data SomeFormat :: Type where
--     SomeFormat :: Sing r -> Sing w -> Format r w -> SomeFormat

data ReaderOptions :: Bool -> Type where
    ROReadable   :: P.ReaderOptions -> ReaderOptions 'True
    ROUnreadable :: ReaderOptions 'False

_ROReadable :: Iso' (ReaderOptions 'True) P.ReaderOptions
_ROReadable = iso (\case ROReadable ro -> ro) ROReadable

_ROUnreadable :: Iso' (ReaderOptions 'False) ()
_ROUnreadable = iso (const ()) (const ROUnreadable)


formatReader
    :: Format 'True w
    -> P.Reader
formatReader = fromJust . (`lookup` P.readers) . readerString

formatWriter
    :: Format r 'True
    -> P.Writer
formatWriter = fromJust . (`lookup` P.writers) . writerString

readerString :: Format 'True w -> String
readerString = \case
    FNative       -> "native"
    FJSON         -> "json"
    FMarkdown mt  -> case mt of
      MDPandoc    -> "markdown"
      MDStrict    -> "markdown_strict"
      MDPHP       -> "markdown_phpextra"
      MDGithub    -> "markdown_github"
      MDMulti     -> "markdown_mmd"
      MDCommon    -> "commonmark"
    FRST          -> "rst"
    FMediaWiki    -> "mediawiki"
    FDocBook      -> "docbook"
    FOPML         -> "opml"
    FOrg          -> "org"
    FTextile      -> "textile"
    FHTML _       -> "html"
    FLaTeX        -> "latex"
    FHaddock      -> "haddock"
    FTWiki        -> "twiki"
    FDocX         -> "docx"
    FODT          -> "odf"
    FT2T          -> "t2t"
    FEPub _       -> "epub"

writerString :: Format r 'True -> String
writerString = \case
    FNative       -> "native"
    FJSON         -> "json"
    FMarkdown mt  -> case mt of
      MDPandoc    -> "markdown"
      MDStrict    -> "markdown_strict"
      MDPHP       -> "markdown_phpextra"
      MDGithub    -> "markdown_github"
      MDMulti     -> "markdown_mmd"
      MDCommon    -> "commonmark"
    FRST          -> "rst"
    FMediaWiki    -> "mediawiki"
    FDocBook      -> "docbook"
    FOPML         -> "opml"
    FOrg          -> "org"
    FTextile      -> "textile"
    FHTML five    -> "html" ++ if five then "5" else ""
    FLaTeX        -> "latex"
    FHaddock      -> "haddock"
    FDocX         -> "docx"
    FODT          -> "odf"
    FEPub three   -> "epub" ++ if three then "3" else ""
    FFictionBook2 -> "fb2"
    FICML         -> "icml"
    FSlideShow ss -> case ss of
      SSS5        -> "s5"
      SSSlidy     -> "slidy"
      SSSlideous  -> "slideous"
      SSDZSlides  -> "dzslides"
      SSRevealJS  -> "revealjs"
      SSBeamer    -> "beamer"
    FOpenDocument -> "opendocument"
    FContext      -> "context"
    FTexinfo      -> "texinfo"
    FMan          -> "man"
    FPlain        -> "plain"
    FDokuWiki     -> "dokuwiki"
    FASCIIDoc     -> "asciidoc"
    FTEI          -> "tei"
