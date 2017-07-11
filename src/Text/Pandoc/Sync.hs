{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}

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

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Dependent.Sum
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Singletons
import           GHC.Generics          (Generic)
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           Text.Pandoc.Sync.File as PS
import qualified Data.Binary           as Bi
import qualified Data.Map              as M
import qualified Data.Set              as S

type FileExt = String

data DiscoverMode = DMSameDir
                  | DMParallelTree (M.Map FilePath FileExt)
  deriving (Show, Eq, Ord, Generic)

data FileDiscover = FD { _fdBaseDir      :: FilePath
                       , _fdFileName     :: String
                       }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''FileDiscover

instance Bi.Binary FileDiscover

data SyncConfig = SC { _scDiscoverMode    :: DiscoverMode
                     , _scDiscoverFormats :: M.Map FileExt (Writer FormatOptions)
                     , _scRoot            :: FilePath
                     , _scCache           :: FilePath
                     }

makeLenses ''SyncConfig

data Sync = Sync { _syncFiles :: M.Map FileDiscover SyncFile }
    deriving (Show, Generic)

makeLenses ''Sync

instance Bi.Binary Sync

initSync :: SyncConfig -> IO Sync
initSync sc = do
    createDirectoryIfMissing True (sc ^. scRoot)
    res <- Sync <$> case sc ^. scDiscoverMode of
      DMSameDir          ->
        M.mapWithKey mkSF <$> discoverAll (sc ^. scDiscoverFormats) (sc ^. scRoot)
      DMParallelTree rts -> fmap (M.unionsWith mergeSF)
                          . traverse (uncurry mkParallel)
                          . M.toList
                          $ rts
    print $ res
    return res
  where
    mkParallel :: FilePath -> FileExt -> IO (M.Map FileDiscover SyncFile)
    mkParallel rt ex = case sc ^? scDiscoverFormats . ix ex of
      -- TODO: handle bad format?
      Nothing -> return M.empty
      Just wf ->
        let fullRt = sc ^. scRoot </> rt
        in  M.mapWithKey mkSF -- . M.mapKeys (over fdFileName (fromJust . stripPrefix fullRt))
              <$> discoverAll (M.singleton ex wf) fullRt
    mkSF :: FileDiscover -> S.Set FileExt -> SyncFile
    mkSF fd exs = emptySyncFile &
        sfSourcesSinks .~ M.mapKeys mkFileName extMap
        -- M.intersectionWith (const go)
        --                                      (M.fromSet (const ()) exs)
        --                                      (sc ^. scDiscoverFormats)
      where
        extMap = M.intersectionWith (const go)
                                    (M.fromSet (const ()) exs)
                                    (sc ^. scDiscoverFormats)
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

addSync :: Sync -> Sync -> Sync
addSync s0 s1 = s0 & syncFiles %~ M.unionWith go (s1 ^. syncFiles)
  where
    go :: SyncFile -> SyncFile -> SyncFile
    go sf1 sf0 = sf0 & sfSourcesSinks %~ (`M.union` (sf1 ^. sfSourcesSinks))

discoverSync :: SyncConfig -> Sync -> IO Sync
discoverSync sc s0 = addSync s0 <$> initSync sc

runSync :: Sync -> IO Sync
runSync = traverseOf (syncFiles . traverse) runSyncFile

loadSync :: SyncConfig -> IO Sync
loadSync sc = do
    s <- tryJust (guard . isDoesNotExistError) $ Bi.decodeFile (sc ^. scCache)
    case s of
      Left ()  -> initSync sc
      Right s' -> discoverSync sc s'

-- caching :: SyncConfig -> (r -> IO Sync) -> r -> IO Sync
-- caching sc f x = do
--     s <- f x
--     Bi.encodeFile (sc ^. scCache) s
--     return s

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
    res <- go rt
    putStrLn $ "Found: " ++ show res
    return res
  where
    go :: FilePath -> IO (M.Map FileDiscover (S.Set FileExt))
    go fp0 = do
        putStrLn $ "Searching directory " ++ fp0
        fmap (M.unionsWith (<>)) . mapM process =<< listDirectory fp0
      where
        process :: FilePath -> IO (M.Map FileDiscover (S.Set FileExt))
        process fp1 = do
          putStrLn $ "Processing " ++ fp1
          isFile <- doesFileExist (fp0 </> fp1)
          let (fn, drop 1 -> ex) = splitExtension fp1
          if isFile
            then return $ if ex `M.member` wfs
                   then M.singleton (FD fp0 fn) (S.singleton ex)
                   else M.empty
            else go (fp0 </> fp1)

mapKeys :: (Ord k1, Ord k2) => Traversal (M.Map k1 v) (M.Map k2 v) k1 k2
mapKeys f = fmap M.fromList . (traverse . _1) f . M.toList
