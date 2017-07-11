{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Pandoc.Sync.Format (
    MarkdownType(..)
  , SlideShowType(..)
  , PDFType(..)
  , Format(..)
  , Writer(..)
  , ReaderOptions(..)
  , readerOptions
  , WriterOptions(..)
  , writerOptions
  , formatReader
  , formatWriter
  , FormatOptions(..)
  , foFormat
  , pdfFormat
  , foWriterOpts
  , foReaderOpts
  , readerString
  , writerString
  , HasIf(..)
  , _Has
  , _Hasn't
  , _HasMaybe
  ) where

import           Control.Lens
import           Data.Default
import           Data.Hashable
import           Data.Kind
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.Prelude.Bool
import           Data.Type.Equality
import           GHC.Generics                 (Generic)
import qualified Data.Binary                  as Bi
import qualified Data.IntMap                  as IM
import qualified Data.Singletons.Decide       as Si
import qualified Text.Pandoc                  as P

data MarkdownType = MDPandoc
                  | MDStrict
                  | MDPHP
                  | MDGithub
                  | MDMulti
                  | MDCommon
  deriving (Show, Generic)

instance Bi.Binary MarkdownType

data SlideShowType = SSS5
                   | SSSlidy
                   | SSSlideous
                   | SSDZSlides
                   | SSRevealJS
                   | SSBeamer
  deriving (Show, Generic)

instance Bi.Binary SlideShowType

data PDFType = PTLaTeX
             | PTBeamer
             | PTContext
             | PTHTML5
  deriving (Show, Generic)

instance Bi.Binary PDFType

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
    FPDF          :: PDFType
                  -> Format 'False 'True

deriving instance Show (Format r w)

instance (SingI r, SingI w) => Bi.Binary (Format r w) where
    get = do
      SomeFormat r w ft <- Bi.get
      Si.Proved Refl <- return $ r Si.%~ (sing @_ @r)
      Si.Proved Refl <- return $ w Si.%~ (sing @_ @w)
      return ft
    put = Bi.put . SomeFormat sing sing

instance (SingI r, SingI w) => Hashable (Format r w) where
    hashWithSalt s fm = s `hashWithSalt` Bi.encode fm

data Writer :: (Bool -> Bool -> Type) -> Type where
    Writer :: SingI r => f r 'True -> Writer f

instance Bi.Binary (Writer FormatOptions) where
    get = do
      r <- Bi.get @Bool
      withSomeSing @_ @Bool r $ \(sr :: Sing r) -> withSingI sr $ do
        Writer @_ @r <$> Bi.get
    put = \case
      Writer (fo :: FormatOptions r 'True) -> do
        Bi.put (fromSing (sing @_ @r))
        Bi.put fo

instance Hashable (Writer FormatOptions) where
    hashWithSalt s = \case
      Writer fo -> s `hashWithSalt` fo

data SomeFormat :: Type where
    SomeFormat :: Sing r -> Sing w -> Format r w -> SomeFormat

instance Bi.Binary P.EPUBVersion

instance Bi.Binary SomeFormat where
    get = do
        i <- Bi.get
        fromMaybe (error "Corrupted SomeFormat") $ IM.lookup i ftmap
      where
        ftmap = IM.fromList [(0, return $ SomeFormat sing sing FNative        )
                            ,(1, return $ SomeFormat sing sing FJSON          )
                            ,(2, SomeFormat sing sing . FMarkdown <$> Bi.get  )
                            ,(3, return $ SomeFormat sing sing FRST           )
                            ,(4, return $ SomeFormat sing sing FMediaWiki     )
                            ,(5, return $ SomeFormat sing sing FDocBook       )
                            ,(6, return $ SomeFormat sing sing FOPML          )
                            ,(7, return $ SomeFormat sing sing FOrg           )
                            ,(8, return $ SomeFormat sing sing FTextile       )
                            ,(9, SomeFormat sing sing . FHTML <$> Bi.get      )
                            ,(10, return $ SomeFormat sing sing FLaTeX        )
                            ,(11, return $ SomeFormat sing sing FHaddock      )
                            ,(12, return $ SomeFormat sing sing FTWiki        )
                            ,(13, return $ SomeFormat sing sing FDocX         )
                            ,(14, return $ SomeFormat sing sing FODT          )
                            ,(15, return $ SomeFormat sing sing FT2T          )
                            ,(16, SomeFormat sing sing . FEPub <$> Bi.get     )
                            ,(17, return $ SomeFormat sing sing FFictionBook2 )
                            ,(18, return $ SomeFormat sing sing FICML         )
                            ,(19, SomeFormat sing sing . FSlideShow <$> Bi.get)
                            ,(20, return $ SomeFormat sing sing FOpenDocument )
                            ,(21, return $ SomeFormat sing sing FContext      )
                            ,(22, return $ SomeFormat sing sing FTexinfo      )
                            ,(23, return $ SomeFormat sing sing FMan          )
                            ,(24, return $ SomeFormat sing sing FPlain        )
                            ,(25, return $ SomeFormat sing sing FDokuWiki     )
                            ,(26, return $ SomeFormat sing sing FASCIIDoc     )
                            ,(27, return $ SomeFormat sing sing FTEI          )
                            ,(28, SomeFormat sing sing . FPDF <$> Bi.get      )
                            ]
    put (SomeFormat _ _ ft) = case ft of
      FNative       -> Bi.put @Int 0
      FJSON         -> Bi.put @Int 1
      FMarkdown mt  -> Bi.put @Int 2 *> Bi.put mt
      FRST          -> Bi.put @Int 3
      FMediaWiki    -> Bi.put @Int 4
      FDocBook      -> Bi.put @Int 5
      FOPML         -> Bi.put @Int 6
      FOrg          -> Bi.put @Int 7
      FTextile      -> Bi.put @Int 8
      FHTML five    -> Bi.put @Int 9 *> Bi.put five
      FLaTeX        -> Bi.put @Int 10
      FHaddock      -> Bi.put @Int 11
      FTWiki        -> Bi.put @Int 12
      FDocX         -> Bi.put @Int 13
      FODT          -> Bi.put @Int 14
      FT2T          -> Bi.put @Int 15
      FEPub v       -> Bi.put @Int 16 *> Bi.put v
      FFictionBook2 -> Bi.put @Int 17
      FICML         -> Bi.put @Int 18
      FSlideShow t  -> Bi.put @Int 19 *> Bi.put t
      FOpenDocument -> Bi.put @Int 20
      FContext      -> Bi.put @Int 21
      FTexinfo      -> Bi.put @Int 22
      FMan          -> Bi.put @Int 23
      FPlain        -> Bi.put @Int 24
      FDokuWiki     -> Bi.put @Int 25
      FASCIIDoc     -> Bi.put @Int 26
      FTEI          -> Bi.put @Int 27
      FPDF t        -> Bi.put @Int 28 *> Bi.put t

instance Hashable SomeFormat where
    hashWithSalt s sf = s `hashWithSalt` Bi.encode sf

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

instance Hashable a => Hashable (HasIf b a) where
    hashWithSalt s = \case
      Has x  -> s `hashWithSalt` (0 :: Int)
                  `hashWithSalt` x
      Hasn't -> s `hashWithSalt` (1 :: Int)

data ReaderOptions = RO
    deriving (Show, Eq, Ord, Generic)

-- data WriterOptions = WO { _woStandalone :: Bool }
data WriterOptions = WO
    deriving (Show, Eq, Ord, Generic)

instance Bi.Binary ReaderOptions
instance Bi.Binary WriterOptions

instance Default ReaderOptions where
    def = RO

instance Default WriterOptions where
    def = WO

instance Hashable ReaderOptions
instance Hashable WriterOptions

data FormatOptions :: Bool -> Bool -> Type where
    FormatOptions :: { _foFormat     :: Format r w
                     , _foReaderOpts :: HasIf r ReaderOptions
                     , _foWriterOpts :: HasIf w WriterOptions
                     }
               -> FormatOptions r w
  deriving (Show, Generic)

makeLenses ''FormatOptions

instance (SingI r, SingI w) => Bi.Binary (FormatOptions r w)

instance (SingI r, SingI w) => Hashable (FormatOptions r w) where
    hashWithSalt s fo = s `hashWithSalt` (fo ^. foFormat)
                          `hashWithSalt` (fo ^. foReaderOpts)
                          `hashWithSalt` (fo ^. foWriterOpts)

readerOptions :: ReaderOptions -> P.ReaderOptions
readerOptions _ = def

writerOptions :: Format r w -> WriterOptions -> P.WriterOptions
writerOptions _ _ = def

pdfFormat :: PDFType -> Writer Format
pdfFormat = \case
    PTLaTeX   -> Writer FLaTeX
    PTBeamer  -> Writer $ FSlideShow SSBeamer
    PTContext -> Writer $ FContext
    PTHTML5   -> Writer $ FHTML True

formatReader
    :: Format 'True w
    -> P.Reader
formatReader = fromMaybe (error "invalid reader string?")
             . (`lookup` P.readers) . readerString

formatWriter
    :: Format r 'True
    -> P.Writer
formatWriter = fromMaybe (error "invalid writer string?")
             . (`lookup` P.writers) . writerString

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
    -- TODO: is this right?
    FPDF t        -> case pdfFormat t of Writer ft -> writerString ft

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
