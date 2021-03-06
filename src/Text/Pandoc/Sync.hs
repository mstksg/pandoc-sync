{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module Text.Pandoc.Sync (
    FileExt
  , DiscoverMode(..)
  , SyncConfig(..)
  , HasSyncConfig(..)
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

-- import           Data.Foldable
-- import           Debug.Trace
-- import qualified Data.Text          as T
import           Control.Applicative
import           Control.Exception
import           Control.Lens hiding   ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Default
import           Data.Dependent.Sum
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Singletons
import           GHC.Generics          (Generic)
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           System.Log.Logger
import           Text.Pandoc.Sync.File as PS
import           Text.Printf
import qualified Data.Binary           as Bi
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Text             as T

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

makeClassy ''SyncConfig

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
        fts   <- (Left  <$> v .: "formats")
             <|> (Right <$> v .: "formats")
        rt    <- v .:? "root"
        cache <- v .:? "cache"
        return $ SC dm
                    (mkFormats fts)
                    (fromMaybe "" rt)
                    (fromMaybe ".pandoc-sync-cache" cache)
      where
        mkFormats
            :: Either (S.Set FileExt) (M.Map FileExt (Writer FormatOptions))
            -> M.Map FileExt (Writer FormatOptions)
        mkFormats (Left  s) = flip M.fromSet s $ \e ->
            case inferWriter e of
              Writer ft -> Writer (FormatOptions ft def def)
        mkFormats (Right m) = m


instance ToJSON SyncConfig where
    toJSON sc = object $ mconcat
        [ case sc ^. scDiscoverMode of
            DMSameDir        -> [ "parallel-mode" .= False ]
            DMParallelTree t -> [ "parallel-mode" .= True
                                , "format-tree"   .= t
                                ]
        , [ "formats" .= (sc ^. scFormats) ]
        , ifNotDef "root"  ""                  (sc ^. scRoot)
        , ifNotDef "cache" "pandoc-sync-cache" (sc ^. scCache)
        ]
      where
        ifNotDef :: (ToJSON a, Eq a) => T.Text -> a -> a -> [(T.Text, Value)]
        ifNotDef t d x | d == x    = []
                       | otherwise = [t .= x]

data Sync = Sync { _syncFiles    :: M.Map FileDiscover SyncFile
                 , _syncConfHash :: Int
                 }
    deriving (Show, Generic)

makeLenses ''Sync

instance Bi.Binary Sync

initSync :: SyncConfig -> IO Sync
initSync sc = do
    createDirectoryIfMissing True (sc ^. scRoot)
    -- TODO: log "new files"
    res <- case sc ^. scDiscoverMode of
      DMSameDir          ->
        fillSameDir . M.mapWithKey mkSF
          <$> discoverAll (sc ^. scFormats) (sc ^. scRoot)
      DMParallelTree rts ->
        M.mapWithKey (fillParallel rts) . M.unionsWith mergeSF
          <$> traverse (uncurry mkParallel) (M.toList rts)
    forM_ (M.toList res) . uncurry $ \fd sf ->
      infoM "pandoc-sync" $ printf "Consolidated file %s, consisting of paths %s"
        (fd ^. fdBaseDir </> fd ^. fdFileName)
        (show (sf ^.. sfSourcesSinks . to M.keys . traverse))
    return $ Sync res (hash sc)
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
    mkParallel :: FilePath -> FileExt -> IO (M.Map FileDiscover SyncFile)
    mkParallel rt ex = case sc ^? scFormats . ix ex of
      -- TODO: handle bad format?
      Nothing -> return M.empty
      Just wf ->
        let fullRt = sc ^. scRoot </> rt
        in  do res <- over (traverse . sfSourcesSinks . mapKeys) (fullRt </>)
                    . M.mapWithKey mkSF
                    . over (mapKeys . fdBaseDir) (fromJust . stripPrefix fullRt)
                    <$> discoverAll (M.singleton ex wf) fullRt
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
        extMap :: M.Map FileExt (DSum Sing SyncFileData)
        extMap = flip M.fromSet exs $ \ex ->
          let d = case inferWriter ex of
                    Writer ft -> Writer $ FormatOptions ft def def
          in  go $ M.findWithDefault d ex (sc ^. scFormats)
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


-- fromJust' :: Show a => (a -> Maybe b) -> a -> b
-- fromJust' f x = fromMaybe (error $ printf "fromJust call") $ f x
--   where
--                   Nothing -> error $ "fromJust called from applying " ++ show x
--                   Just y  -> y

addSync :: Sync -> Sync -> Sync
addSync s0 s1 = s0 & syncFiles %~ M.unionWith go (s1 ^. syncFiles)
  where
    go :: SyncFile -> SyncFile -> SyncFile
    go sf1 sf0 = sf0 & sfSourcesSinks %~ (`M.union` (sf1 ^. sfSourcesSinks))

discoverSync :: SyncConfig -> Sync -> IO Sync
discoverSync sc s0 = addSync s0 <$> initSync sc

-- TODO: on first run, ConflictMode should be newest
runSync :: Bool -> ConflictMode -> ConflictMode -> Sync -> IO Sync
runSync dry cm0 cm1 = itraverseOf (syncFiles . itraversed) $ \fd sf -> do
    debugM "pandoc-sync" $ printf "Syncing file %s"
      (fd ^. fdBaseDir </> fd ^. fdFileName)
    runSyncFile dry cm0 cm1 sf

loadSync :: SyncConfig -> IO Sync
loadSync sc = do
    es <- fmap join
        . tryJust (guard . isDoesNotExistError)
        . set (mapped . _Left) ()
        $ Bi.decodeFileOrFail (sc ^. scCache)
    case es of
      Right s | s ^. syncConfHash == hash sc ->
        discoverSync sc s
              | otherwise                    -> do
        warningM "pandoc-sync" "Configuration changed"
        removeFile (sc ^. scCache)
        initSync sc
      Left ()                                ->
        initSync sc

withSync :: SyncConfig -> (Sync -> IO (r, Sync)) -> IO r
withSync sc f = do
    s0 <- loadSync sc
    (y, s1) <- f s0
    Bi.encodeFile (sc ^. scCache) s1
    return y

withSync_ :: SyncConfig -> (Sync -> IO Sync) -> IO ()
withSync_ sc f = withSync sc (fmap ((),) . f)

discoverAll
    :: M.Map FileExt (Writer FormatOptions)
    -> FilePath
    -> IO (M.Map FileDiscover (S.Set FileExt))
discoverAll wfs rt = do
    isFile <- doesFileExist rt
    when isFile $ error "There's a file there, what gives?"
    createDirectoryIfMissing True rt
    debugM "pandoc-sync" $ printf "Discovering files in %s with extensions in %s"
        rt (show (M.keys wfs))
    res <- go rt
    forM_ (M.toList res) . uncurry $ \fd exs ->
      debugM "pandoc-sync" $ printf "Found file %s with extensions %s"
        (fd ^. fdBaseDir </> fd ^. fdFileName)
        (show (S.toList exs))
    return res
  where
    go :: FilePath -> IO (M.Map FileDiscover (S.Set FileExt))
    go fp0 = do
        debugM "pandoc-sync" $ printf "Searching directory %s" fp0
        fmap (M.unionsWith (<>)) . mapM process . filter unHidden =<< listDirectory fp0
      where
        -- ignoring hidden files.
        unHidden p = not $ "." `isPrefixOf` takeFileName p
        process :: FilePath -> IO (M.Map FileDiscover (S.Set FileExt))
        process fp1 = do
          let fullPath = fp0 </> fp1
          debugM "pandoc-sync" $ printf "Processing file %s" fullPath
          isFile <- doesFileExist fullPath
          let (fn, drop 1 -> ex) = splitExtension fp1
          if isFile
            then return $ if ex `M.member` wfs
                   then M.singleton (FD fp0 fn) (S.singleton ex)
                   else M.empty
            else go fullPath

mapKeys :: (Ord k1, Ord k2) => Traversal (M.Map k1 v) (M.Map k2 v) k1 k2
mapKeys f = fmap M.fromList . (traverse . _1) f . M.toList
