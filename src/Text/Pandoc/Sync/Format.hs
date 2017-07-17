{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Pandoc.Sync.Format (
    MarkdownType(..)
  , SlideShowType(..), AsSlideShowType(..)
  , PDFType(..)
  , Format(..), AsFormat(..)
  , SomeFormat(..), someFormat
  , asReader
  , writeOnly
  , Writer(..)
  , ReaderOptions(..)
  , fromReaderOptions
  , WriterOptions(..), HasWriterOptions(..)
  -- , writerOptions
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
  , hasIfMaybe
  , allFormats
  , allWriters
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Default
import           Data.Foldable
import           Data.Hashable
import           Data.Kind
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.Prelude.Bool
import           Data.Type.Equality
import           GHC.Generics                 (Generic)
import qualified Data.Binary                  as Bi
import qualified Data.IntMap                  as IM
import qualified Data.Map                     as M
import qualified Data.Singletons.Decide       as Si
import qualified Text.Megaparsec              as MP
import qualified Text.Megaparsec.Lexer        as MPL
import qualified Text.Megaparsec.Text         as MP
import qualified Text.Pandoc                  as P

data MarkdownType = MDPandoc
                  | MDStrict
                  | MDPHP
                  | MDGithub
                  | MDMulti
                  | MDCommon
  deriving (Show, Generic, Eq, Ord, Enum, Bounded)

instance Bi.Binary MarkdownType

data SlideShowType = SSS5
                   | SSSlidy
                   | SSSlideous
                   | SSDZSlides
                   | SSRevealJS
                   | SSBeamer
  deriving (Show, Generic, Eq, Ord, Enum, Bounded)

makeClassyPrisms ''SlideShowType

instance Bi.Binary SlideShowType

data PDFType = PTLaTeX
             | PTBeamer
             | PTConTeXt
             | PTHTML5
  deriving (Show, Generic, Eq, Ord, Enum, Bounded)

instance Bi.Binary PDFType

data Format :: Bool -> Bool -> Type where
    FNative       :: Format 'True  'True
    FJSON         :: Format 'True  'True
    FMarkdown     :: MarkdownType
                  -> Format 'True  'True
    FRST          :: Format 'True  'True
    FMediaWiki    :: Format 'True  'True
    FDocBook      :: Bool
                  -> Format 'True  'True
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
    FConTeXt      :: Format 'False 'True
    FTexinfo      :: Format 'False 'True
    FMan          :: Format 'False 'True
    FPlain        :: Format 'False 'True
    FDokuWiki     :: Format 'False 'True
    FZimWiki      :: Format 'False 'True
    FASCIIDoc     :: Format 'False 'True
    FTEI          :: Format 'False 'True
    FPDF          :: PDFType
                  -> Format 'False 'True
    FRTF          :: Format 'False 'True
    FMkWriteOnly  :: Format 'True  'True
                  -> Format 'False 'True
    FMkReadOnly   :: Format 'True  'True
                  -> Format 'True  'False

deriving instance Show (Format r w)

instance (SingI r, SingI w) => Bi.Binary (Format r w) where
    get = do
      SomeFormat r w ft <- Bi.get
      Si.Proved Refl <- return $ r Si.%~ (sing @_ @r)
      Si.Proved Refl <- return $ w Si.%~ (sing @_ @w)
      return ft
    put = Bi.put . someFormat

instance (SingI r, SingI w) => Hashable (Format r w) where
    hashWithSalt s fm = s `hashWithSalt` Bi.encode fm

class AsFormat ft where
    _FHTML      :: Prism' (ft 'True 'True ) Bool
    _FSlideShow :: Prism' (ft 'False 'True) SlideShowType
    _FPDF       :: Prism' (ft 'False 'True) PDFType

instance AsFormat Format where
    _FHTML = prism FHTML (\case FHTML t -> Right t
                                ft      -> Left ft
                         )
    _FSlideShow = prism FSlideShow (\case FSlideShow t -> Right t
                                          ft           -> Left ft
                                   )
    _FPDF = prism FPDF (\case FPDF t -> Right t
                              ft     -> Left ft
                       )

asReader :: forall r' r w. SingI r' => Sing r -> Traversal' (Format r' w) (Format r w)
asReader sr f = case sing @_ @r' Si.%~ sr of
    Si.Proved Refl -> f
    Si.Disproved _ -> pure

writeOnly :: forall r. SingI r => Format r 'True -> Format 'False 'True
writeOnly = case sing @_ @r of
    SFalse -> id
    STrue  -> FMkWriteOnly

data Writer :: (Bool -> Bool -> Type) -> Type where
    Writer :: SingI r => f r 'True -> Writer f

instance FromJSON (Writer Format) where
    parseJSON v = do
      SomeFormat sr sw ft <- parseJSON v
      case sw of
        SFalse -> fail "Unwritable format given where writable format is expected"
        STrue  -> withSingI sr $ return (Writer ft)

instance Show (Writer Format) where
    showsPrec d (Writer ft) = showParen (d > app_prec) $
        showString "Writer " . showsPrec (app_prec + 1) ft
      where
        app_prec = 10

data SomeFormat :: Type where
    SomeFormat :: Sing r -> Sing w -> Format r w -> SomeFormat

someFormat :: (SingI r, SingI w) => Format r w -> SomeFormat
someFormat = SomeFormat sing sing

instance Show SomeFormat where
    showsPrec d (SomeFormat _ _ ft) = showParen (d > app_prec) $
        showString "SomeFormat " . showsPrec (app_prec + 1) ft
      where
        app_prec = 10

instance Bi.Binary P.EPUBVersion

instance Bi.Binary SomeFormat where
    get = do
        i <- Bi.get
        fromMaybe (error "Corrupted SomeFormat") $ IM.lookup i ftmap
      where
        ftmap = IM.fromList [(0, return $ someFormat FNative          )
                            ,(1, return $ someFormat FJSON            )
                            ,(2, someFormat . FMarkdown <$> Bi.get    )
                            ,(3, return $ someFormat FRST             )
                            ,(4, return $ someFormat FMediaWiki       )
                            ,(5, someFormat . FDocBook <$> Bi.get     )
                            ,(6, return $ someFormat FOPML            )
                            ,(7, return $ someFormat FOrg             )
                            ,(8, return $ someFormat FTextile         )
                            ,(9, someFormat . FHTML <$> Bi.get        )
                            ,(10, return $ someFormat FLaTeX          )
                            ,(11, return $ someFormat FHaddock        )
                            ,(12, return $ someFormat FTWiki          )
                            ,(13, return $ someFormat FDocX           )
                            ,(14, return $ someFormat FODT            )
                            ,(15, return $ someFormat FT2T            )
                            ,(16, someFormat . FEPub <$> Bi.get       )
                            ,(17, return $ someFormat FFictionBook2   )
                            ,(18, return $ someFormat FICML           )
                            ,(19, someFormat . FSlideShow <$> Bi.get  )
                            ,(20, return $ someFormat FOpenDocument   )
                            ,(21, return $ someFormat FConTeXt        )
                            ,(22, return $ someFormat FTexinfo        )
                            ,(23, return $ someFormat FMan            )
                            ,(24, return $ someFormat FPlain          )
                            ,(25, return $ someFormat FDokuWiki       )
                            ,(26, return $ someFormat FASCIIDoc       )
                            ,(27, return $ someFormat FTEI            )
                            ,(28, someFormat . FPDF <$> Bi.get        )
                            ,(29, return $ someFormat FZimWiki        )
                            ,(30, return $ someFormat FRTF            )
                            ,(31, someFormat . FMkWriteOnly <$> Bi.get)
                            ,(32, someFormat . FMkReadOnly <$> Bi.get )
                            ]
    put (SomeFormat _ _ ft) = case ft of
      FNative        -> Bi.put @Int 0
      FJSON          -> Bi.put @Int 1
      FMarkdown mt   -> Bi.put @Int 2 *> Bi.put mt
      FRST           -> Bi.put @Int 3
      FMediaWiki     -> Bi.put @Int 4
      FDocBook v     -> Bi.put @Int 5 *> Bi.put v
      FOPML          -> Bi.put @Int 6
      FOrg           -> Bi.put @Int 7
      FTextile       -> Bi.put @Int 8
      FHTML five     -> Bi.put @Int 9 *> Bi.put five
      FLaTeX         -> Bi.put @Int 10
      FHaddock       -> Bi.put @Int 11
      FTWiki         -> Bi.put @Int 12
      FDocX          -> Bi.put @Int 13
      FODT           -> Bi.put @Int 14
      FT2T           -> Bi.put @Int 15
      FEPub v        -> Bi.put @Int 16 *> Bi.put v
      FFictionBook2  -> Bi.put @Int 17
      FICML          -> Bi.put @Int 18
      FSlideShow t   -> Bi.put @Int 19 *> Bi.put t
      FOpenDocument  -> Bi.put @Int 20
      FConTeXt       -> Bi.put @Int 21
      FTexinfo       -> Bi.put @Int 22
      FMan           -> Bi.put @Int 23
      FPlain         -> Bi.put @Int 24
      FDokuWiki      -> Bi.put @Int 25
      FASCIIDoc      -> Bi.put @Int 26
      FTEI           -> Bi.put @Int 27
      FPDF t         -> Bi.put @Int 28 *> Bi.put t
      FZimWiki       -> Bi.put @Int 29
      FRTF           -> Bi.put @Int 30
      FMkWriteOnly f -> Bi.put @Int 31 *> Bi.put f
      FMkReadOnly f  -> Bi.put @Int 32 *> Bi.put f

instance Hashable SomeFormat where
    hashWithSalt s (SomeFormat sr sw ft) = withSingI sr $
                                           withSingI sw $
      hashWithSalt s ft

data HasIf :: Bool -> Type -> Type where
    Has    :: a -> HasIf 'True a
    Hasn't :: HasIf 'False a

deriving instance Show a => Show (HasIf b a)

_Has :: Iso' (HasIf 'True a) a
_Has = iso (\case Has x -> x) Has

_Hasn't :: Iso' (HasIf 'False a) ()
_Hasn't = iso (const ()) (const Hasn't)

hasIfMaybe :: HasIf b a -> Maybe a
hasIfMaybe = \case
    Has  x -> Just x
    Hasn't -> Nothing

-- _HasMaybe :: forall r a b. SingI r => Prism (HasIf r a) (HasIf r b) a b
-- _HasMaybe = prism (case sing @_ @r of
--                      STrue -> Has
--                      SFalse -> const Hasn't
--                   )
--                   (\case Has x -> Right x; Hasn't -> Left Hasn't)

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

instance (SingI b, FromJSON a) => FromJSON (HasIf b a) where
    parseJSON = case sing @_ @b of
      STrue  -> fmap Has . parseJSON
      SFalse -> const $ pure Hasn't

data ReaderOptions = RO
    deriving (Show, Eq, Ord, Generic)

deriving instance Ord P.HTMLMathMethod
instance Bi.Binary P.HTMLMathMethod
instance Hashable P.HTMLMathMethod

data WriterOptions = WO { _woStandalone   :: Bool
                        , _woTemplatePath :: Maybe FilePath
                        , _woDataDir      :: Maybe FilePath
                        , _woMathMethod   :: P.HTMLMathMethod
                        , _woVariables    :: M.Map String String
                        , _woTabStop      :: Int
                        , _woTOC          :: Bool
                        }
    deriving (Show, Eq, Ord, Generic)

makeClassy ''WriterOptions

instance Bi.Binary ReaderOptions
instance Bi.Binary WriterOptions

instance Default ReaderOptions where
    def = RO

instance Default WriterOptions where
    def = WO True Nothing Nothing P.PlainMath M.empty 4 False

instance Hashable ReaderOptions
instance Hashable WriterOptions where
    hashWithSalt s wo = s `hashWithSalt` wo ^. woStandalone
                          `hashWithSalt` wo ^. woTemplatePath
                          `hashWithSalt` wo ^. woDataDir
                          `hashWithSalt` M.toList (wo ^. woVariables)
                          `hashWithSalt` wo ^. woTabStop
                          `hashWithSalt` wo ^. woTOC

instance FromJSON ReaderOptions
instance FromJSON WriterOptions where
    parseJSON = withObject "WriterOptions" $ \v ->
      WO <$> (fromMaybe True    <$> v .:? "standalone")
         <*> (v .:? "template-path")
         <*> (v .:? "data-dir")
         <*> pure P.PlainMath
         <*> (fromMaybe M.empty <$> v .:? "variables")
         <*> (fromMaybe 4 <$> v .: "tab-stop")
         <*> (fromMaybe False <$> (v .: "toc" <|> v .: "table-of-contents"))

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

-- instance (SingI r, SingI w) => FromJSON (FormatOptions r w) where

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

instance FromJSON (Writer FormatOptions) where
    parseJSON v = withOpts v <|> noOpts v
      where
        withOpts = withObject "Writer FormatOptions" $ \v' -> do
          Writer ft <- v' .: "format"
          ro <- v' .:? "opts"
          wo <- v' .:? "opts"
          return . Writer $
            FormatOptions ft (fromMaybe def ro) (fromMaybe def wo)
        noOpts t = parseJSON t <&> \case
          Writer ft -> Writer (FormatOptions ft def def)
      

instance Show (Writer FormatOptions) where
    showsPrec d (Writer fo) = showParen (d > app_prec) $
        showString "Writer " . showsPrec (app_prec + 1) fo
      where
        app_prec = 10

fromReaderOptions :: ReaderOptions -> P.ReaderOptions
fromReaderOptions _ = def

-- data FormatOptions :: Bool -> Bool -> Type where
--     FormatOptions :: { _foFormat     :: Format r w
--                      , _foReaderOpts :: HasIf r ReaderOptions
--                      , _foWriterOpts :: HasIf w WriterOptions
--                      }
--                -> FormatOptions r w
--   deriving (Show, Generic)

-- writerOptions :: Format r w -> WriterOptions -> P.WriterOptions
-- writerOptions _ _ = def

pdfFormat :: PDFType -> Writer Format
pdfFormat = \case
    PTLaTeX   -> Writer FLaTeX
    PTBeamer  -> Writer $ FSlideShow SSBeamer
    PTConTeXt -> Writer $ FConTeXt
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
    FDocBook _    -> "docbook"
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
    FMkReadOnly f -> readerString f         -- any way to indicate?

writerString :: Format r 'True -> String
writerString = \case
    FNative        -> "native"
    FJSON          -> "json"
    FMarkdown mt   -> case mt of
      MDPandoc     -> "markdown"
      MDStrict     -> "markdown_strict"
      MDPHP        -> "markdown_phpextra"
      MDGithub     -> "markdown_github"
      MDMulti      -> "markdown_mmd"
      MDCommon     -> "commonmark"
    FRST           -> "rst"
    FMediaWiki     -> "mediawiki"
    FDocBook five  -> "docbook" ++ if five then "5" else ""
    FOPML          -> "opml"
    FOrg           -> "org"
    FTextile       -> "textile"
    FHTML five     -> "html" ++ if five then "5" else ""
    FLaTeX         -> "latex"
    FHaddock       -> "haddock"
    FDocX          -> "docx"
    FODT           -> "odf"
    FEPub epv      -> "epub" ++ case epv of P.EPUB2 -> ""; P.EPUB3 -> "3"
    FFictionBook2  -> "fb2"
    FICML          -> "icml"
    FSlideShow ss  -> case ss of
      SSS5         -> "s5"
      SSSlidy      -> "slidy"
      SSSlideous   -> "slideous"
      SSDZSlides   -> "dzslides"
      SSRevealJS   -> "revealjs"
      SSBeamer     -> "beamer"
    FOpenDocument  -> "opendocument"
    FConTeXt       -> "context"
    FTexinfo       -> "texinfo"
    FMan           -> "man"
    FPlain         -> "plain"
    FDokuWiki      -> "dokuwiki"
    FASCIIDoc      -> "asciidoc"
    FTEI           -> "tei"
    -- TODO: is this right?
    FPDF t         -> case pdfFormat t of Writer ft -> writerString ft
    FZimWiki       -> "zimwiki"
    FRTF           -> "rtf"
    FMkWriteOnly f -> writerString f            -- any way to indicate?

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

allFormats :: [SomeFormat]
allFormats = concat [ all'
                    , someFormat . FMkWriteOnly <$> mapMaybe rw all'
                    , someFormat . FMkReadOnly  <$> mapMaybe rw all'
                    ]
  where
    all' = concat
      [ [ someFormat FNative
        , someFormat FJSON
        ]
      , someFormat . FMarkdown <$> [minBound .. maxBound]
      , [ someFormat FRST
        , someFormat FMediaWiki
        ]
      , someFormat . FDocBook <$> [minBound .. maxBound]
      , [ someFormat FOPML
        , someFormat FOrg
        , someFormat FTextile
        ]
      , someFormat . FHTML <$> [minBound .. maxBound]
      , [ someFormat FLaTeX
        , someFormat FHaddock
        , someFormat FTWiki
        , someFormat FDocX
        , someFormat FODT
        , someFormat FT2T
        ]
      , someFormat . FEPub <$> [P.EPUB2, P.EPUB3]
      , [ someFormat FFictionBook2
        , someFormat FICML
        ]
      , someFormat . FSlideShow <$> [minBound .. maxBound]
      , [ someFormat FOpenDocument
        , someFormat FConTeXt
        , someFormat FTexinfo
        , someFormat FMan
        , someFormat FPlain
        , someFormat FDokuWiki
        , someFormat FZimWiki
        , someFormat FASCIIDoc
        , someFormat FTEI
        ]
      , someFormat . FPDF <$> [minBound .. maxBound]
      , [ someFormat FRTF
        ]
      ]
    rw = filterSomeFormat STrue STrue

allWriters :: [Writer Format]
allWriters = mapMaybe go allFormats
  where
    go x = (Writer <$> filterSomeFormat SFalse STrue x)
       <|> (Writer <$> filterSomeFormat STrue  STrue x)


filterSomeFormat
    :: Sing r
    -> Sing w
    -> SomeFormat
    -> Maybe (Format r w)
filterSomeFormat sr sw (SomeFormat sr' sw' ft) = do
    Refl <- decideMaybe $ sr Si.%~ sr'
    Refl <- decideMaybe $ sw Si.%~ sw'
    return ft
  where
    decideMaybe :: Si.Decision a -> Maybe a
    decideMaybe = \case
      Si.Proved x    -> Just x
      Si.Disproved _ -> Nothing

parseSomeFormat :: MP.Parser SomeFormat
parseSomeFormat = do
    baseFormat <- asum [ MP.try singleWord
                       , someFormat . FMarkdown <$> MP.try markdown
                       ]
    modFlag <- asum [ MP.try $ someFormat . FMkReadOnly  <$ symbol "readonly"
                    , MP.try $ someFormat . FMkReadOnly  <$ symbol "ro"
                    , MP.try $ someFormat . FMkWriteOnly <$ symbol "writeonly"
                    , MP.try $ someFormat . FMkWriteOnly <$ symbol "wo"
                    , pure $ someFormat
                    ]
    return $ case baseFormat of
       SomeFormat STrue STrue ft -> modFlag ft
       SomeFormat sr    sw    ft -> SomeFormat sr sw ft
  where
    symbol = MPL.symbol' MP.space
    markdown :: MP.Parser MarkdownType
    markdown = do
      _ <- MP.try (symbol "markdown") <|> symbol "md"
      asum [ MP.try $ MDStrict <$ symbol "strict"
           , MP.try $ MDPHP    <$ symbol "phpextra"
           , MP.try $ MDPHP    <$ symbol "php"
           , MP.try $ MDGithub <$ symbol "github"
           , MP.try $ MDGithub <$ symbol "gh"
           , MP.try $ MDMulti  <$ symbol "mmd"
           , MP.try $ MDMulti  <$ symbol "multi"
           , MP.try $ MDMulti  <$ symbol "multimarkdown"
           , MP.try $ MDCommon <$ symbol "common"
           , MP.try $ MDCommon <$ symbol "commonmark"
           , pure MDPandoc
           ]
    singleWord :: MP.Parser SomeFormat
    singleWord = asum . flip M.mapWithKey singleWords $ \s ft -> MP.try $
      ft <$ symbol s
    singleWords :: M.Map String SomeFormat
    singleWords = M.fromList
      [ ("native"      , someFormat FNative                )
      , ("haskell"     , someFormat FNative                )
      , ("json"        , someFormat FJSON                  )
      , ("common"      , someFormat (FMarkdown MDCommon)   )
      , ("commonmark"  , someFormat (FMarkdown MDCommon)   )
      , ("html"        , someFormat (FHTML True)           )
      , ("html4"       , someFormat (FHTML False)          )
      , ("html5"       , someFormat (FHTML True)           )
      , ("html-pdf"    , someFormat (FPDF PTHTML5)         )
      , ("html5-pdf"   , someFormat (FPDF PTHTML5)         )
      , ("epub"        , someFormat (FEPub P.EPUB2)        )
      , ("epub2"       , someFormat (FEPub P.EPUB2)        )
      , ("epub3"       , someFormat (FEPub P.EPUB3)        )
      , ("docbook"     , someFormat (FDocBook False)       )
      , ("docbook5"    , someFormat (FDocBook True)        )
      , ("rst"         , someFormat FRST                   )
      , ("mediawiki"   , someFormat FMediaWiki             )
      , ("opml"        , someFormat FOPML                  )
      , ("org"         , someFormat FOrg                   )
      , ("textile"     , someFormat FTextile               )
      , ("latex"       , someFormat FLaTeX                 )
      , ("tex"         , someFormat FLaTeX                 )
      , ("pdf"         , someFormat (FPDF PTLaTeX)         )
      , ("latex-pdf"   , someFormat (FPDF PTLaTeX)         )
      , ("tex-pdf"     , someFormat (FPDF PTLaTeX)         )
      , ("haddock"     , someFormat FHaddock               )
      , ("twiki"       , someFormat FTWiki                 )
      , ("docx"        , someFormat FDocX                  )
      , ("doc"         , someFormat FDocX                  )
      , ("odt"         , someFormat FODT                   )
      , ("t2t"         , someFormat FT2T                   )
      , ("fb2"         , someFormat FFictionBook2          )
      , ("fb"          , someFormat FFictionBook2          )
      , ("fictionbook" , someFormat FFictionBook2          )
      , ("fictionbook2", someFormat FFictionBook2          )
      , ("icml"        , someFormat FICML                  )
      , ("opendocument", someFormat FOpenDocument          )
      , ("context"     , someFormat FConTeXt               )
      , ("context-pdf" , someFormat (FPDF PTConTeXt)       )
      , ("texinfo"     , someFormat FTexinfo               )
      , ("man"         , someFormat FMan                   )
      , ("plain"       , someFormat FPlain                 )
      , ("dokuwiki"    , someFormat FDokuWiki              )
      , ("doku"        , someFormat FDokuWiki              )
      , ("zimwiki"     , someFormat FZimWiki               )
      , ("zim"         , someFormat FZimWiki               )
      , ("asciidoc"    , someFormat FASCIIDoc              )
      , ("tei"         , someFormat FTEI                   )
      , ("rtf"         , someFormat FRTF                   )
      , ("s5"          , someFormat (FSlideShow SSS5)      )
      , ("slidy"       , someFormat (FSlideShow SSSlidy)   )
      , ("slideous"    , someFormat (FSlideShow SSSlideous))
      , ("dz"          , someFormat (FSlideShow SSDZSlides))
      , ("dzslides"    , someFormat (FSlideShow SSDZSlides))
      , ("revealjs"    , someFormat (FSlideShow SSRevealJS))
      , ("reveal"      , someFormat (FSlideShow SSRevealJS))
      , ("beamer"      , someFormat (FPDF PTBeamer)        )
      , ("beamer-pdf"  , someFormat (FPDF PTBeamer)        )
      , ("beamer-latex", someFormat (FSlideShow SSBeamer)  )
      , ("beamer-tex"  , someFormat (FSlideShow SSBeamer)  )
      ]

instance FromJSON SomeFormat where
    parseJSON = withText "SomeFormat" $ \t ->
      case MP.parse parseSomeFormat "Value" t of
        Left  e  -> fail $ MP.parseErrorPretty e
        Right sf -> return sf

