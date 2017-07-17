{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Text.Pandoc.Sync (
    FileExt
  , DiscoverMode(..)
  , SyncConfig(..)
  , FileDiscover(..)
  , Sync(..)
  , loadSync
  , initSync
  , runSync
  , discoverSync
  , withSync
  , withSync_
  , module PS
  ) where

-- import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Dependent.Sum
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Singletons
import           Debug.Trace
import           GHC.Generics           (Generic)
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           Text.Pandoc.Sync.File  as PS
import           Text.Printf
import qualified Data.Binary            as Bi
import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.Text              as T

type FileExt = String

data DiscoverMode = DMSameDir
                  | DMParallelTree (M.Map FilePath FileExt)
  deriving (Show, Eq, Ord, Generic)

instance Bi.Binary DiscoverMode
instance Hashable DiscoverMode where
    hashWithSalt s = \case
      DMSameDir         -> s `hashWithSalt` (0 :: Int)
      DMParallelTree mp -> s `hashWithSalt` (1 :: Int)
                             `hashWithSalt` M.toList mp

-- instance FromJSON DiscoverMode where
--     parseJSON = withObject "DiscoverMode" $ \v -> do
--       mode <- v .: "mode"
--       case mode :: T.Text of
--         "same-dir" -> pure DMSameDir
--         "parallel" -> DMParallelTree <$> v .: "format-tree"


data FileDiscover = FD { _fdBaseDir      :: FilePath
                       , _fdFileName     :: String
                       }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''FileDiscover

instance Bi.Binary FileDiscover

data SyncConfig = SC { _scDiscoverMode :: DiscoverMode
                     , _scFormats      :: M.Map FileExt (Writer FormatOptions)
                     , _scRoot         :: FilePath
                     , _scCache        :: FilePath
                     }
  deriving (Show, Generic)

makeLenses ''SyncConfig

instance Bi.Binary SyncConfig
instance Hashable SyncConfig where
    hashWithSalt s sc = s `hashWithSalt` (sc ^. scDiscoverMode)
                          `hashWithSalt` (sc ^. scRoot)
                          `hashWithSalt` (sc ^. scCache)
                          `hashWithSalt` M.toList (sc ^. scFormats)


instance FromJSON SyncConfig where
    parseJSON = withObject "SyncConfig" $ \v -> do
      pmode <- v .:? "parallel-mode"
      ftree <- v .:? "format-tree"
      dm <- case pmode of
        Just True -> case ftree of
          Just t  -> return $ DMParallelTree t
          Nothing -> fail "Parallel mode indicated, but format-tree required."
        Just False -> return DMSameDir
        Nothing -> case ftree of
          Just t  -> return $ DMParallelTree t
          Nothing -> return DMSameDir
      fts   <- v .: "formats"
      rt    <- v .:? "root"
      cache <- v .:? "cache"
      return $ SC dm
                  fts
                  (fromMaybe "" rt)
                  (fromMaybe ".pandoc-sync-cache" cache)

data Sync = Sync { _syncFiles    :: M.Map FileDiscover SyncFile
                 , _syncConfHash :: Int
                 }
    deriving (Show, Generic)

makeLenses ''Sync

instance Bi.Binary Sync

initSync :: forall m. MonadLoggerIO m => SyncConfig -> m Sync
initSync sc = do
    liftIO $ createDirectoryIfMissing True (sc ^. scRoot)
    res <- flip Sync (hash sc) <$> case sc ^. scDiscoverMode of
      DMSameDir          ->
        fillSameDir . M.mapWithKey mkSF
          <$> discoverAll (sc ^. scFormats) (sc ^. scRoot)
      DMParallelTree rts ->
        M.mapWithKey (fillParallel rts) . join traceShow . M.unionsWith mergeSF
          <$> traverse (uncurry mkParallel) (M.toList rts)
    forOf_ (syncFiles . to M.toList . traverse) res . uncurry $ \fd sf -> do
      logDebugN ""
    return res
  where
    fillSameDir
        :: M.Map FileDiscover SyncFile
        -> M.Map FileDiscover SyncFile
    fillSameDir sfs = M.unionWith mergeSF sfs fills
      where
        fills :: M.Map FileDiscover SyncFile
        fills = M.mapWithKey go sfs
        go :: FileDiscover -> SyncFile -> SyncFile
        go fd = over sfSourcesSinks (`M.union` filled)
          where
            filled :: M.Map FilePath (DSum Sing SyncFileData)
            filled = M.fromList . map (uncurry go2) . M.toList $ sc ^. scFormats
            go2 :: FileExt
                -> Writer FormatOptions
                -> (FilePath, DSum Sing SyncFileData)
            go2 ex (Writer fo) = (fullPath, sfd)
              where
                sfd = sing :=> SyncFileData (fo ^. foFormat)
                                            (fo ^. foReaderOpts)
                                            (fo ^. foWriterOpts . _Has)
                                            Nothing
                fullPath = fd ^. fdBaseDir </> fd ^. fdFileName -<.> ex
    mkParallel :: FilePath -> FileExt -> m (M.Map FileDiscover SyncFile)
    mkParallel rt ex = case sc ^? scFormats . ix ex of
      -- TODO: handle bad format?
      Nothing -> return M.empty
      Just wf ->
        let fullRt = sc ^. scRoot </> rt
        in  do res <- over (traverse . sfSourcesSinks . mapKeys) (fullRt </>)
                    . M.mapWithKey mkSF
                    . over (mapKeys . fdBaseDir) (fromJust' (stripPrefix fullRt))
                    <$> discoverAll (M.singleton ex wf) fullRt
               -- liftIO $ do
               --   putStrLn "Making parallel:"
               --   putStrLn fullRt
               --   print res
               return res
    fillParallel :: M.Map FilePath FileExt -> FileDiscover -> SyncFile -> SyncFile
    fillParallel rts fd = over sfSourcesSinks (`M.union` filled)
      where
        filled = M.fromList
               . mapMaybe (uncurry go)
               . M.toList
               $ rts
        go :: FilePath -> FileExt -> Maybe (FilePath, DSum Sing SyncFileData)
        go rt ex = sc ^? scFormats
                       . ix ex
                       . to (\case Writer fo ->
                                    let sfd = sing :=> SyncFileData (fo ^. foFormat)
                                                  (fo ^. foReaderOpts)
                                                  (fo ^. foWriterOpts . _Has)
                                                  Nothing
                                    in  (fullPath, sfd)
                            )
          where
            fullPath = sc ^. scRoot </> rt </> fd ^. fdBaseDir </> fd ^. fdFileName -<.> ex
    mkSF :: FileDiscover -> S.Set FileExt -> SyncFile
    mkSF fd exs = emptySyncFile &
        sfSourcesSinks .~ M.mapKeys mkFileName extMap
      where
        extMap = M.intersectionWith (const go)
                                    (M.fromSet (const ()) exs)
                                    (sc ^. scFormats)
        mkFileName :: FileExt -> FilePath
        mkFileName fe = (fd ^. fdBaseDir) </> (fd ^. fdFileName) -<.> fe
        go :: Writer FormatOptions -> DSum Sing SyncFileData
        go = \case
          Writer fo -> sing :=>
            SyncFileData (fo ^. foFormat)
                         (fo ^. foReaderOpts)
                         (fo ^. foWriterOpts . _Has)
                         Nothing
    mergeSF :: SyncFile -> SyncFile -> SyncFile
    mergeSF s1 s2 = s1 & sfSourcesSinks %~ (`M.union` (s2 ^. sfSourcesSinks))


fromJust' :: Show a => (a -> Maybe b) -> a -> b
fromJust' f x = case f x of
                  Nothing -> error $ "fromJust called from applying " ++ show x
                  Just y  -> y

addSync :: Sync -> Sync -> Sync
addSync s0 s1 = s0 & syncFiles %~ M.unionWith go (s1 ^. syncFiles)
  where
    go :: SyncFile -> SyncFile -> SyncFile
    go sf1 sf0 = sf0 & sfSourcesSinks %~ (`M.union` (sf1 ^. sfSourcesSinks))

discoverSync :: MonadLoggerIO m => SyncConfig -> Sync -> m Sync
discoverSync sc s0 = addSync s0 <$> initSync sc

runSync :: MonadLoggerIO m => Sync -> m Sync
runSync = traverseOf (syncFiles . traverse) runSyncFile

loadSync :: MonadLoggerIO m => SyncConfig -> m Sync
loadSync sc = do
    es <- liftIO
        . fmap join
        . tryJust (guard . isDoesNotExistError)
        . set (mapped . _Left) ()
        $ Bi.decodeFileOrFail (sc ^. scCache)
    case es of
      Right s | s ^. syncConfHash == hash sc ->
        discoverSync sc s
              | otherwise                    -> do
        logInfoN "Configuration changed"
        liftIO $ removeFile (sc ^. scCache)
        initSync sc
      Left ()                                ->
        initSync sc

withSync :: MonadLoggerIO m => SyncConfig -> (Sync -> m (r, Sync)) -> m r
withSync sc f = do
    s0 <- loadSync sc
    (y, s1) <- f s0
    liftIO $ Bi.encodeFile (sc ^. scCache) s1
    return y

withSync_ :: MonadLoggerIO m => SyncConfig -> (Sync -> m Sync) -> m ()
withSync_ sc f = withSync sc (fmap ((),) . f)

discoverAll
    :: forall m. MonadLoggerIO m
    => M.Map FileExt (Writer FormatOptions)
    -> FilePath
    -> m (M.Map FileDiscover (S.Set FileExt))
discoverAll wfs rt = do
    liftIO $ do
      isFile <- doesFileExist rt
      when isFile $ error "There's a file there, what gives?"
      createDirectoryIfMissing True rt
    res <- go rt
    forM_ (M.toList res) . uncurry $ \fd exs ->
      logDebugN . T.pack $ printf "Found file %s with extensions %s"
        (fd ^. fdBaseDir </> fd ^. fdFileName)
        (show (S.toList exs))
    return res
  where
    go :: FilePath -> m (M.Map FileDiscover (S.Set FileExt))
    go fp0 = do
        logDebugN . T.pack $ printf "Searching directory %s" fp0
        fmap (M.unionsWith (<>)) . mapM process =<< liftIO (listDirectory fp0)
      where
        process :: FilePath -> m (M.Map FileDiscover (S.Set FileExt))
        process fp1 = do
          liftIO $ putStrLn $ "Processing " ++ fp1
          isFile <- liftIO $ doesFileExist (fp0 </> fp1)
          let (fn, drop 1 -> ex) = splitExtension fp1
          if isFile
            then return $ if ex `M.member` wfs
                   then M.singleton (FD fp0 fn) (S.singleton ex)
                   else M.empty
            else go (fp0 </> fp1)

mapKeys :: (Ord k1, Ord k2) => Traversal (M.Map k1 v) (M.Map k2 v) k1 k2
mapKeys f = fmap M.fromList . (traverse . _1) f . M.toList
