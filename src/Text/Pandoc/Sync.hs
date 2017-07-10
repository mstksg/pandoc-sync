{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Text.Pandoc.Sync (
    MarkdownType(..)
  , SlideShowType(..)
  , Format(..)
  , ReaderOptions(..)
  , SyncData(..)
  , Sync(..)
  , runSync
  , discoverSync
  , discoverAndRunSync
  , initAndDiscoverSync
  , initAndRunSync
  ) where

import           Control.Lens hiding        ((<.>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.Kind
import           Data.Maybe hiding          (catMaybes)
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Time.Clock
import           Data.Tuple
import           Data.Type.Conjunction
import           Data.Witherable
import           System.Directory
import           System.FilePath
import           Type.Class.Higher
import qualified Data.ByteString.Lazy       as B
import qualified Data.Map                   as M
import qualified Data.OrdPSQ                as PS
import qualified Text.Pandoc                as P
import qualified Text.Pandoc.MediaBag       as P
import qualified Text.Pandoc.PDF            as P
import qualified Text.Pandoc.Readers.LaTeX  as P
import qualified Text.Pandoc.SelfContained  as P
import qualified Text.Pandoc.Shared         as P
import qualified Text.Pandoc.UTF8           as UTF8

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
    FNative       :: Format 'True 'True
    FJSON         :: Format 'True 'True
    FMarkdown     :: MarkdownType -> Format 'True 'True
    FRST          :: Format 'True 'True
    FMediaWiki    :: Format 'True 'True
    FDocBook      :: Format 'True 'True
    FOPML         :: Format 'True 'True
    FOrg          :: Format 'True 'True
    FTextile      :: Format 'True 'True
    FHTML         :: Bool -> Format 'True 'True
    FLaTeX        :: Format 'True 'True
    FHaddock      :: Format 'True 'True
    FTWiki        :: Format 'True 'False
    FDocX         :: Format 'True 'True
    FODT          :: Format 'True 'True
    FT2T          :: Format 'True 'False
    FEPub         :: Bool -> Format 'True 'True
    FFictionBook2 :: Format 'False 'True
    FICML         :: Format 'False 'True
    FSlideShow    :: SlideShowType -> Format 'False 'True
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

_ROReadable :: Lens' (ReaderOptions 'True) P.ReaderOptions
_ROReadable f = \case
    ROReadable ro -> ROReadable <$> f ro

data SyncData :: Bool -> Type where
    SyncData :: { _sdFormat     :: Format r 'True
                , _sdReaderOpts :: ReaderOptions r
                , _sdWriterOpts :: P.WriterOptions
                , _sdLastSync   :: Maybe UTCTime
                }
             -> SyncData r

makeLenses ''SyncData

data DiscoverMode = DMSameDir     (M.Map WriterFormat String) FilePath
                  | DMParallelDir (M.Map WriterFormat (FilePath, String))


data Sync = Sync
    { _syncSources    :: M.Map FilePath (SyncData 'True )
    , _syncSinks      :: M.Map FilePath (SyncData 'False)
    , _syncLastUpdate :: Maybe (UTCTime, P.Pandoc)
    , _syncDiscover   :: Maybe (String, DiscoverMode)
    }

makeLenses ''Sync

syncSourcesSinks :: Lens' Sync (M.Map FilePath (Some (Sing :&: SyncData)))
syncSourcesSinks f s0 = f bigMap <&> \bm ->
    let (newSources, newSinks) = M.mapEither breakUp bm
    in  s0 & syncSources .~ newSources
           & syncSinks   .~ newSinks
  where
    bigMap = M.union (g <$> (s0 ^. syncSources)) (g <$> (s0 ^. syncSinks))
    g :: SingI r => SyncData r -> Some (Sing :&: SyncData)
    g sd = Some (sing :&: sd)
    breakUp :: Some (Sing :&: SyncData) -> Either (SyncData 'True) (SyncData 'False)
    breakUp = \case
      Some (STrue :&: sd)  -> Left sd
      Some (SFalse :&: sd) -> Right sd

discoverSync :: Sync -> IO Sync
discoverSync s0 = do
    news <- fmap (maybe M.empty (M.fromList . map swap . M.toList . catMaybes))
          . forM (s0 ^. syncDiscover) . uncurry $ \fname mode ->
      forM (discFiles fname mode) $ \fp -> runMaybeT $ do
        guard $ M.notMember fp (s0 ^. syncSources)
        guard $ M.notMember fp (s0 ^. syncSinks  )
        guard =<< liftIO (doesPathExist fp)
        return fp
    return $ s0 & syncSourcesSinks %~ (`M.union` M.map mkNew news)
  where
    discFiles :: String -> DiscoverMode -> M.Map WriterFormat FilePath
    discFiles fname = \case
      DMSameDir fs d   -> fs <&> \ext      -> d </> fname </> ext
      DMParallelDir fs -> fs <&> \(d, ext) -> d </> fname </> ext
    mkNew :: WriterFormat -> Some (Sing :&: SyncData)
    mkNew = \case
      WriterFormat fm ->
        let sB = sing
            ro = case sB of
              SFalse -> ROUnreadable
              STrue  -> ROReadable P.def
        in  Some (sB :&: SyncData fm ro P.def Nothing)

runSync :: Sync -> IO Sync
runSync s0 = do
    syncTime <- getCurrentTime
    (updatesE, updates) <- fmap (M.mapEither id . catMaybes)
             . ifor (s0 ^. syncSources) $ \fp sd -> runMaybeT $ do
      modTime <- liftIO $ getModificationTime fp
      guard $ anyOf (sdLastSync . _Just) (< modTime) sd
      liftIO . (fmap . fmap) ((sd ^. sdLastSync,) . fst) $
        readPandoc (sd ^. sdFormat) (sd ^. sdReaderOpts . _ROReadable) fp
    let sortedUpdates = mapToQueue id updates
    case PS.minView sortedUpdates of
      Nothing -> do
        putStrLn "No updates found"
        return s0
      Just (fp, _, pd, laterUpdates) -> do
        putStrLn "Updates found!"
        itraverse_ backupError updatesE
        itraverse_ backupLater $ queueToMap (flip const) laterUpdates
        s0 & syncLastUpdate                  .~ Just (syncTime, pd)
           & syncSourcesSinks . itraversed %%@~ \fp' (Some (s :&: sd)) -> do
               sd' <- updateSource syncTime (fp == fp') pd fp' sd
               return $ Some (s :&: sd')
  where
    backupError :: FilePath -> P.PandocError -> IO ()
    backupError _ _ = return ()
    backupLater :: FilePath -> P.Pandoc -> IO ()
    backupLater _ _ = return ()
    updateSource :: UTCTime -> Bool -> P.Pandoc -> FilePath -> SyncData r -> IO (SyncData r)
    updateSource st skipWrite pd fp sd = do
      unless skipWrite $ case formatWriter (sd ^. sdFormat) of
        P.IOStringWriter f ->
          UTF8.writeFile fp                =<< f (sd ^. sdWriterOpts) pd
        P.IOByteStringWriter f ->
          B.writeFile (UTF8.encodePath fp) =<< f (sd ^. sdWriterOpts) pd
        P.PureStringWriter f -> case pdfEngine (sd ^. sdFormat) of
          Just eng -> do
            -- TODO: handle lack of prog?
            -- Just mbPdfProg <- findExecutable eng
            res <- P.makePDF eng f (sd ^. sdWriterOpts) pd
            -- TODO: handle bad res?
            traverse_ (B.writeFile (UTF8.encodePath fp)) res
          Nothing -> do
              let res = f (sd ^. sdWriterOpts) pd
              out <- if htmlFormat (sd ^. sdFormat)
                 then P.makeSelfContained (sd ^. sdWriterOpts) res
                 else return res
              UTF8.writeFile fp out

      return $ sd & sdLastSync .~ Just st

discoverAndRunSync :: Sync -> IO Sync
discoverAndRunSync = runSync <=< discoverSync

initSync :: Either () (String, DiscoverMode) -> Sync
initSync = \case
    -- TODO: support
    Left ()  -> Sync M.empty M.empty Nothing Nothing
    Right dm -> Sync M.empty M.empty Nothing (Just dm)

initAndDiscoverSync
    :: Either () (String, DiscoverMode)
    -> IO Sync
initAndDiscoverSync = discoverSync . initSync

initAndRunSync
    :: Either () (String, DiscoverMode)
    -> IO Sync
initAndRunSync = discoverAndRunSync . initSync

mapToQueue
    :: (Ord k, Ord p)
    => (a -> (p, b))
    -> M.Map k a
    -> PS.OrdPSQ k p b
mapToQueue f = PS.fromList . (map . uncurry) g . M.toList
  where
    g k (f->(p, v)) = (k, p, v)

queueToMap
    :: Ord k
    => (p -> a -> b)
    -> PS.OrdPSQ k p a
    -> M.Map k b
queueToMap f = M.fromList . map g . PS.toList
  where
    g (k, p, v) = (k, f p v)

-- TODO: customizable latex engine
pdfEngine
    :: Format r w
    -> Maybe String
pdfEngine = \case
    FLaTeX              -> Just "pdflatex"
    FSlideShow SSBeamer -> Just "pdflatex"
    FContext            -> Just "context"
    FHTML True          -> Just "wkhtmltopdf"
    _                   -> Nothing

htmlFormat
    :: Format r w
    -> Bool
htmlFormat = \case
    FHTML _ -> True
    FSlideShow ss -> case ss of
      SSBeamer -> False
      _        -> True
    _       -> False

readPandoc
    :: Format 'True w
    -> P.ReaderOptions
    -> FilePath
    -> IO (Either P.PandocError (P.Pandoc, P.MediaBag))
readPandoc ft ro fp = case formatReader ft of
    P.StringReader r -> runExceptT $ do
      src <- convertTabs <$> liftIO (readFile fp)
      doc <- ExceptT $ handleIncludes src
      out <- ExceptT $ r ro doc
      return (out, mempty)
    P.ByteStringReader r ->
      r ro =<< B.readFile fp
  where
    convertTabs = P.tabFilter $ case ft of
      FT2T -> 0
      _    -> P.readerTabStop ro
    handleIncludes :: String -> IO (Either P.PandocError String)
    handleIncludes = case ft of
      FLaTeX -> P.handleIncludes
      _      -> return . Right

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

