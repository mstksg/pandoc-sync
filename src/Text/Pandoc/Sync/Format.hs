{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}

module Text.Pandoc.Sync.Format (
    MarkdownType(..)
  , SlideShowType(..)
  , Format(..)
  , Writer(..)
  , ReaderOptions(..)
  , readerOptions
  , WriterOptions(..)
  , writerOptions
  , formatReader
  , formatWriter
  , FormatOptions
  , foFormat
  , foWriterOpts
  , foReaderOpts
  , readerString
  , writerString
  , HasIf
  , _Has
  , _Hasn't
  , _HasMaybe
  ) where

import           Control.Lens
import           Data.Default
import           Data.Kind
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.Prelude.Bool
import           GHC.Generics                 (Generic)
import qualified Data.Binary                  as Bi
import qualified Text.Pandoc                  as P

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
    FEPub         :: P.EPUBVersion
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

instance Bi.Binary (Format r w) where
    get = undefined
    put = undefined

-- data ReaderFormat :: Type where
--     ReaderFormat :: SingI w => Format 'True w -> ReaderFormat

data Writer :: (Bool -> Bool -> Type) -> Type where
    Writer :: SingI r => f r 'True -> Writer f

-- data SomeFormat :: Type where
--     SomeFormat :: Sing r -> Sing w -> Format r w -> SomeFormat

data HasIf :: Bool -> Type -> Type where
    Has    :: a -> HasIf 'True a
    Hasn't :: HasIf 'False a

deriving instance Show a => Show (HasIf b a)

_Has :: Iso' (HasIf 'True a) a
_Has = iso (\case Has x -> x) Has

_Hasn't :: Iso' (HasIf 'False a) ()
_Hasn't = iso (const ()) (const Hasn't)

_HasMaybe :: forall r a b. SingI r => Prism (HasIf r a) (HasIf r b) a b
_HasMaybe = prism (case sing @_ @r of
                     STrue -> Has
                     SFalse -> const Hasn't
                  )
                  (\case Has x -> Right x; Hasn't -> Left Hasn't)

instance (SingI b, Bi.Binary a) => Bi.Binary (HasIf b a) where
    get = case sing @_ @b of
      STrue  -> Has <$> Bi.get
      SFalse -> return Hasn't
    put = \case
      Has x  -> Bi.put x
      Hasn't -> return ()

instance (SingI b, Default a) => Default (HasIf b a) where
    def = case sing @_ @b of
      STrue  -> Has def
      SFalse -> Hasn't

data ReaderOptions = RO
    deriving (Show, Eq, Ord, Generic)

data WriterOptions = WO
    deriving (Show, Eq, Ord, Generic)

instance Bi.Binary ReaderOptions
instance Bi.Binary WriterOptions

instance Default ReaderOptions where
    def = RO

instance Default WriterOptions where
    def = WO

data FormatOptions :: Bool -> Bool -> Type where
    FormatOptions :: { _foFormat     :: Format r w
                     , _foReaderOpts :: HasIf r ReaderOptions
                     , _foWriterOpts :: HasIf w WriterOptions
                     }
               -> FormatOptions r w
  deriving (Show, Generic)

makeLenses ''FormatOptions


readerOptions :: ReaderOptions -> P.ReaderOptions
readerOptions _ = def

writerOptions :: WriterOptions -> P.WriterOptions
writerOptions _ = def

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
    FEPub epv     -> "epub" ++ case epv of P.EPUB2 -> ""; P.EPUB3 -> ""
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

-- formatString :: Format r w -> String
-- formatString = \case
--     FNative       -> "native"
--     FJSON         -> "json"
--     FMarkdown mt  -> case mt of
--       MDPandoc    -> "markdown"
--       MDStrict    -> "markdown_strict"
--       MDPHP       -> "markdown_phpextra"
--       MDGithub    -> "markdown_github"
--       MDMulti     -> "markdown_mmd"
--       MDCommon    -> "commonmark"
--     FRST          -> "rst"
--     FMediaWiki    -> "mediawiki"
--     FDocBook      -> "docbook"
--     FOPML         -> "opml"
--     FOrg          -> "org"
--     FTextile      -> "textile"
--     FHTML five    -> "html" ++ if five then "5" else ""
--     FLaTeX        -> "latex"
--     FHaddock      -> "haddock"
--     FDocX         -> "docx"
--     FODT          -> "odf"
--     FEPub epv     -> "epub" ++ case epv of P.EPUB2 -> ""; P.EPUB3 -> ""
--     FFictionBook2 -> "fb2"
--     FICML         -> "icml"
--     FSlideShow ss -> case ss of
--       SSS5        -> "s5"
--       SSSlidy     -> "slidy"
--       SSSlideous  -> "slideous"
--       SSDZSlides  -> "dzslides"
--       SSRevealJS  -> "revealjs"
--       SSBeamer    -> "beamer"
--     FOpenDocument -> "opendocument"
--     FContext      -> "context"
--     FTexinfo      -> "texinfo"
--     FMan          -> "man"
--     FPlain        -> "plain"
--     FDokuWiki     -> "dokuwiki"
--     FASCIIDoc     -> "asciidoc"
--     FTEI          -> "tei"
--     FTWiki        -> "twiki"
--     FT2T          -> "t2t"
