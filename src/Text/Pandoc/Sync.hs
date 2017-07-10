{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}

module Text.Pandoc.Sync (
    FileExt
  , DiscoverMode(..)
  , SyncConfig(..)
  , FileDiscover(..)
  , Sync(..)
  , initSync
  , discoverSync
  , module PS
  ) where

import           Control.Lens
import           Data.Default
import           Data.Dependent.Sum
import           Data.Monoid
import           Data.Singletons
import           Data.Witherable
import           GHC.Generics            (Generic)
import           System.Directory
import           System.FilePath
import           Text.Pandoc.Sync.File   as PS
import           Text.Pandoc.Sync.Format as PS
import qualified Data.Binary             as Bi
import qualified Data.Map                as M
import qualified Data.Set                as S

type FileExt = String

data DiscoverMode = DMSameDir
                  | DMParallelTree (M.Map FilePath FileExt)

data FileDiscover = FD { _fdBaseDir      :: FilePath
                       , _fdFileName     :: String
                       }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''FileDiscover

instance Bi.Binary FileDiscover

data SyncConfig = SC { _scDiscoverMode    :: DiscoverMode
                     , _scDiscoverFormats :: M.Map FileExt WriterFormat
                     }

makeLenses ''SyncConfig

data Sync = Sync { _syncFiles :: M.Map FileDiscover SyncFile }
    deriving Generic

makeLenses ''Sync

instance Bi.Binary Sync

initSync :: SyncConfig -> IO Sync
initSync sc = Sync <$>
    case sc ^. scDiscoverMode of
      DMSameDir          ->
        M.mapWithKey mkSF <$> discoverAll (sc ^. scDiscoverFormats) ""
      DMParallelTree rts -> fmap (M.unionsWith mergeSF)
                          . traverse (uncurry mkParallel)
                          . M.toList
                          $ rts
  where
    mkParallel :: FilePath -> FileExt -> IO (M.Map FileDiscover SyncFile)
    mkParallel fp ex = case sc ^? scDiscoverFormats . ix ex of
      -- TODO: handle bad format?
      Nothing -> return M.empty
      Just wf -> M.mapWithKey mkSF <$> discoverAll (M.singleton ex wf) fp
    mkSF :: FileDiscover -> S.Set FileExt -> SyncFile
    mkSF fd exs = emptySyncFile &
        sfSourcesSinks .~ M.intersectionWith (const go)
                                             (M.fromSet (const ()) exs)
                                             (sc ^. scDiscoverFormats)
      where
        go :: WriterFormat -> DSum Sing SyncFileData
        go = \case
          WriterFormat ft -> sing :=> SyncFileData ft def def Nothing
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

discoverAll
    :: M.Map FileExt WriterFormat
    -> FilePath
    -> IO (M.Map FileDiscover (S.Set FileExt))
discoverAll wfs = go
  where
    go :: FilePath -> IO (M.Map FileDiscover (S.Set FileExt))
    go fp0 = fmap (M.unionsWith (<>)) . mapM process =<< listDirectory fp0
      where
        process :: FilePath -> IO (M.Map FileDiscover (S.Set FileExt))
        process fp1 = do
          isFile <- doesFileExist fp1
          let (fn, drop 1 -> ex) = splitExtension fp1
          if isFile
            then return $ if ex `M.member` wfs
                   then M.empty
                   else M.singleton (FD fp0 fn) (S.singleton ex)
            else go (fp0 </> fp1)

mapKeys :: (Ord k1, Ord k2) => Traversal (M.Map k1 v) (M.Map k2 v) k1 k2
mapKeys f = fmap M.fromList . (traverse . _1) f . M.toList
