{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}

module Text.Pandoc.Sync.File (
    SyncFileData(..)
  , SyncFile(..)
  , sfSourcesSinks
  , emptySyncFile
  , runSyncFile
  ) where

import           Control.Lens hiding        ((<.>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Dependent.Sum
import           Data.Foldable
import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Time.Clock
import           Data.Witherable
import           System.Directory
import           Text.Pandoc.Sync.Format
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


data SyncFileData :: Bool -> Type where
    SyncFileData :: { _sfdFormat     :: Format r 'True
                    , _sfdReaderOpts :: ReaderOptions r
                    , _sfdWriterOpts :: P.WriterOptions
                    , _sfdLastSync   :: Maybe UTCTime
                    }
                 -> SyncFileData r

makeLenses ''SyncFileData

-- data DiscoverMode = DMSameDir     (M.Map WriterFormat String) FilePath
--                   | DMParallelDir (M.Map WriterFormat (FilePath, String))

data SyncFile = SyncFile
    { _sfSources    :: M.Map FilePath (SyncFileData 'True )
    , _sfSinks      :: M.Map FilePath (SyncFileData 'False)
    , _sfLastUpdate :: Maybe (UTCTime, P.Pandoc)
    -- , _syncDiscover   :: Maybe (String, DiscoverMode)
    }

makeLenses ''SyncFile

emptySyncFile :: SyncFile
emptySyncFile = SyncFile M.empty M.empty Nothing

sfSourcesSinks :: Lens' SyncFile (M.Map FilePath (DSum Sing SyncFileData))
sfSourcesSinks f s0 = f bigMap <&> \bm ->
    let (newSources, newSinks) = M.mapEither breakUp bm
    in  s0 & sfSources .~ newSources
           & sfSinks   .~ newSinks
  where
    bigMap = M.union (g <$> (s0 ^. sfSources)) (g <$> (s0 ^. sfSinks))
    g :: SingI r => SyncFileData r -> DSum Sing SyncFileData
    g sfd = sing :=> sfd
    breakUp :: DSum Sing SyncFileData -> Either (SyncFileData 'True) (SyncFileData 'False)
    breakUp = \case
      STrue  :=> sfd -> Left sfd
      SFalse :=> sfd -> Right sfd

-- discoverSyncFile :: SyncFile -> IO SyncFile
-- discoverSyncFile s0 = do
--     news <- fmap (maybe M.empty (M.fromList . map swap . M.toList . catMaybes))
--           . forM (s0 ^. syncDiscover) . uncurry $ \fname mode ->
--       forM (discFiles fname mode) $ \fp -> runMaybeT $ do
--         guard $ M.notMember fp (s0 ^. sfSources)
--         guard $ M.notMember fp (s0 ^. sfSinks  )
--         guard =<< liftIO (doesPathExist fp)
--         return fp
--     return $ s0 & sfSourcesSinks %~ (`M.union` M.map mkNew news)
--   where
--     discFiles :: String -> DiscoverMode -> M.Map WriterFormat FilePath
--     discFiles fname = \case
--       DMSameDir fs d   -> fs <&> \ext      -> d </> fname </> ext
--       DMParallelDir fs -> fs <&> \(d, ext) -> d </> fname </> ext
--     mkNew :: WriterFormat -> Some (Sing :&: SyncFileData)
--     mkNew = \case
--       WriterFormat fm ->
--         let sB = sing
--             ro = case sB of
--               SFalse -> ROUnreadable
--               STrue  -> ROReadable P.def
--         in  Some (sB :&: SyncFileData fm ro P.def Nothing)

runSyncFile :: SyncFile -> IO SyncFile
runSyncFile s0 = do
    syncTime <- getCurrentTime
    (updatesE, updates) <- fmap (M.mapEither id . catMaybes)
             . ifor (s0 ^. sfSources) $ \fp sfd -> runMaybeT $ do
      modTime <- liftIO $ getModificationTime fp
      guard $ anyOf (sfdLastSync . _Just) (< modTime) sfd
      liftIO . (fmap . fmap) ((sfd ^. sfdLastSync,) . fst) $
        readPandoc (sfd ^. sfdFormat) (sfd ^. sfdReaderOpts . _ROReadable) fp
    let sortedUpdates = mapToQueue id updates
    case PS.minView sortedUpdates of
      Nothing -> do
        putStrLn "No updates found"
        return s0
      Just (fp, _, pd, laterUpdates) -> do
        putStrLn "Updates found!"
        itraverse_ backupError updatesE
        itraverse_ backupLater $ queueToMap (flip const) laterUpdates
        s0 & sfLastUpdate                  .~ Just (syncTime, pd)
           & sfSourcesSinks . itraversed %%@~ \fp' (s :=> sfd) -> do
               sfd' <- updateSource syncTime (fp == fp') pd fp' sfd
               return $ s :=> sfd'
  where
    backupError :: FilePath -> P.PandocError -> IO ()
    backupError _ _ = return ()
    backupLater :: FilePath -> P.Pandoc -> IO ()
    backupLater _ _ = return ()
    updateSource :: UTCTime -> Bool -> P.Pandoc -> FilePath -> SyncFileData r -> IO (SyncFileData r)
    updateSource st skipWrite pd fp sfd = do
      unless skipWrite $ case formatWriter (sfd ^. sfdFormat) of
        P.IOStringWriter f ->
          UTF8.writeFile fp                =<< f (sfd ^. sfdWriterOpts) pd
        P.IOByteStringWriter f ->
          B.writeFile (UTF8.encodePath fp) =<< f (sfd ^. sfdWriterOpts) pd
        P.PureStringWriter f -> case pdfEngine (sfd ^. sfdFormat) of
          Just eng -> do
            -- TODO: handle lack of prog?
            -- Just mbPdfProg <- findExecutable eng
            res <- P.makePDF eng f (sfd ^. sfdWriterOpts) pd
            -- TODO: handle bad res?
            traverse_ (B.writeFile (UTF8.encodePath fp)) res
          Nothing -> do
              let res = f (sfd ^. sfdWriterOpts) pd
              out <- if htmlFormat (sfd ^. sfdFormat)
                 then P.makeSelfContained (sfd ^. sfdWriterOpts) res
                 else return res
              UTF8.writeFile fp out

      return $ sfd & sfdLastSync .~ Just st

-- discoverAndRunSync :: Sync -> IO Sync
-- discoverAndRunSync = runSync <=< discoverSync

-- initSync :: Either () (String, DiscoverMode) -> Sync
-- initSync = \case
--     -- TODO: support
--     Left ()  -> Sync M.empty M.empty Nothing Nothing
--     Right dm -> Sync M.empty M.empty Nothing (Just dm)

-- initAndDiscoverSync
--     :: Either () (String, DiscoverMode)
--     -> IO Sync
-- initAndDiscoverSync = discoverSync . initSync

-- initAndRunSync
--     :: Either () (String, DiscoverMode)
--     -> IO Sync
-- initAndRunSync = discoverAndRunSync . initSync

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



