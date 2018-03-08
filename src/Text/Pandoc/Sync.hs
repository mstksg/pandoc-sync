{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeInType        #-}

module Text.Pandoc.Sync (
    Config
  , classify
  ) where

-- import           Control.Alternative.Free.Final
import           Control.Lens
import           Control.Monad.Operational
import           Data.Aeson
import           Data.Default
import           Data.Kind
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Time
import           System.FilePath
import           Text.Pandoc.Sync.Format
import qualified Data.Aeson.Types                  as A
import qualified Data.ByteString                   as BS
import qualified Data.Map                          as M
import qualified Data.Map.Merge.Lazy               as M
import qualified Data.Set                          as S
import qualified Data.Text                         as T
import qualified Text.Pandoc.Definition            as P

type CanPath = FilePath

data BranchSpec = BS { _bsRoot   :: FilePath
                     , _bsFormat :: SomeFormat
                     }
  deriving (Show, Eq, Ord)

data CFile = CF { _cfPandoc :: P.Pandoc
                , _cfUpdate :: (UTCTime, BranchSpec)
                }
  deriving (Show, Eq, Ord)

-- data BFile = BF { _bfCFile  :: FilePath         -- ^ canonical file?
--                 , _bfUpdate :: UTCTime
--                 }

type Canonical = M.Map CanPath CFile

type Branch    = M.Map CanPath UTCTime

data SyncState = SS { _ssCanonical :: Canonical
                    , _ssBranches  :: M.Map BranchSpec Branch
                    }

data Opts = O
  deriving Show

instance Semigroup Opts where
    _ <> _ = O

instance Monoid Opts where
    mempty = O

data FileConfig = FC { _fcOpts     :: Opts
                     , _fcBranches :: M.Map BranchSpec Opts
                     }
  deriving Show

data Config = C { _cOpts     :: Opts
                , _cBranches :: M.Map BranchSpec Opts
                , _cFiles    :: M.Map CanPath FileConfig
                }
  deriving Show

instance Default Opts where
    def = O

collapseMap
    :: (Ord c, Semigroup d)
    => (a -> b -> c)
    -> M.Map a (M.Map b d)
    -> M.Map c d
collapseMap f m = M.fromListWith (<>) $ do
    (x, ys) <- M.toList m
    (y, z ) <- M.toList ys
    return (f x y, z)


instance FromJSON FileConfig where
    parseJSON = withObject "FileConfig" $ \v -> do
      opts     <- v .:? "options"  .!= def
      branches <- v .:? "branches" .!= M.empty
      return $ FC opts (collapseMap BS branches)

instance FromJSON Opts where
    parseJSON = withObject "Opts" $ \v ->
      pure O

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> do
        opts     <- v .:? "options"  .!= def
        branches <- v .:? "branches" .!= M.empty
        files    <- v .:? "files"    .!= M.empty
        return $ C opts (collapseMap BS branches) files

classify
    :: Config
    -> FilePath
    -> M.Map BranchSpec CanPath     -- canonical
classify C{..} fp = M.fromList $ do
    rootElems <- drop 1 . reverse . inits . splitPath $ fp
    let root = takeDirectory . joinPath $ rootElems
        bs   = BS root format
        can  = dropExtension . makeRelative root $ fp
    _ <- maybeToList $ M.lookup bs _cBranches
    return (bs, can)
  where
    format = inferFormat (drop 1 (takeExtension fp))

-- data CatF :: Type -> Type where
--     CatFile :: FilePath -> CatF (Maybe T.Text)

-- type Cat = Alt CatF


-- Data flow:
--
-- 1. Get all file changes
-- 2. Update branch info with file changes
-- 3. Update canonical with branch info
-- 4. Propagate canonical to branch info
-- 5. Sync filesystem with branch info
--
-- Maybe try the 'git' style of storing states and diffing on-the-fly:
--
-- 1. Update all branches based on filesystem
-- 2. Aggregate branches into canonical representation
-- 3. Propagate canonical representations to branches
-- 4. Sync filesystem with branches

-- data FileUpdate = FU { _fuTime :: UTCTime
--                      , _fuBody :: BS.ByteString
--                      }

updateBranch
    :: CanPath
    -> UTCTime
    -> Branch
    -> Branch
updateBranch p t = set (at p) (Just t)

updateCanonical
    :: M.Map BranchSpec Branch
    -> Canonical
    -> Canonical
updateCanonical bs = M.merge newPath noUpdate updated paths
  where
    paths :: M.Map CanPath (M.Map UTCTime (S.Set BranchSpec))
    paths = (M.unionsWith . M.unionWith) S.union
          . ifoldMap (\sp -> (:[]) . fmap (flip M.singleton (S.singleton sp)))
          $ bs
    newPath :: M.SimpleWhenMissing CanPath (M.Map UTCTime (S.Set BranchSpec)) CFile
    newPath = M.mapMaybeMissing $ \p us -> CF _ _
    noUpdate :: M.SimpleWhenMissing CanPath CFile CFile
    noUpdate = M.preserveMissing
    updated :: M.SimpleWhenMatched CanPath (M.Map UTCTime (S.Set BranchSpec)) CFile CFile
    updated = undefined

    -- -> Branch
    -- -> Canonical
    -- -> Canonical
-- updateCanonical
-- data SyncState = SS { _ssCanonical :: Canonical
    --                 , _ssBranches  :: M.Map BranchSpec Branch
    --                 }


-- type Canonical = M.Map CanPath CFile

-- type Branch    = M.Map CanPath UTCTime

-- data SyncState = SS { _ssCanonical :: Canonical
--                     , _ssBranches  :: M.Map BranchSpec Branch
--                     }

-- data FileUpdate = FU { _fuUpdate   :: UTCTime
--                      , _fuContents :: BS.ByteString
--                      }

-- data UpdateType = UTChange
--                 | UTAdd



-- updateSync
--     :: FileUpdate
--     -> SyncState
--     -> (SyncState, [Int])
-- updateSync FU{..} SS{..} = _

-- data Update = FileChange FilePath UTCTime BS.ByteString
--             | FileAdd    FilePath UTCTime BS.ByteString

