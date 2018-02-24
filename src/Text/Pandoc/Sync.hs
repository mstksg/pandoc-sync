{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.Pandoc.Sync (
    Config
  ) where

import           Data.Aeson
import           Data.List
import           Data.Maybe
import           Data.Time
import           Data.Default
import           System.FilePath
import           Text.Pandoc.Sync.Format
import qualified Data.Aeson.Types        as A
import qualified Data.ByteString         as BS
import qualified Data.Map                as M
import qualified Text.Pandoc.Definition  as P

data BranchSpec = BS { _bsRoot   :: FilePath
                     , _bsFormat :: SomeFormat
                     }
  deriving (Show, Eq, Ord)

data CFile = CF { _cfPath   :: FilePath
                , _cfPandoc :: P.Pandoc
                , _cfUpdate :: (UTCTime, BranchSpec)
                }

data BFile = BF { _bfPath   :: FilePath         -- ^ relative to root
                , _bfCFile  :: FilePath
                , _bfUpdate :: UTCTime
                }

type Canonical = M.Map FilePath CFile

type Branch    = M.Map FilePath BFile

-- type BranchSpecMap a = M.Map FilePath (M.Map SomeFormat a)

data SyncState = SS { _ssCanonical :: Canonical
                    , _ssBranches  :: M.Map BranchSpec Branch
                    }

data FileUpdate = FU { _fuUpdate   :: UTCTime
                     , _fuContents :: BS.ByteString
                     }

data UpdateType = UTChange
                | UTAdd

data Opts = O
  deriving Show

data FileConfig = FC { _fcOpts     :: Opts
                     , _fcBranches :: M.Map BranchSpec Opts
                     }
  deriving Show

data Config = C { _cOpts     :: Opts
                , _cBranches :: M.Map BranchSpec Opts
                , _cFiles    :: M.Map FilePath FileConfig
                }
  deriving Show

instance Default Opts where
    def = O

collapseMap
    :: Ord c
    => (a -> b -> c)
    -> M.Map a (M.Map b d)
    -> M.Map c d
collapseMap f m = M.fromList $ do
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
    -> [(BranchSpec, FilePath)]     -- canonical
classify C{..} fp = do
    rootElems <- drop 1 . reverse . inits . splitPath $ fp
    let root = takeDirectory . joinPath $ rootElems
        bs   = BS root format
        can  = dropExtension . makeRelative root $ fp
    _ <- maybeToList $ M.lookup bs _cBranches
    return (bs, can)
  where
    format = inferFormat (drop 1 (takeExtension fp))

-- updateSync
--     :: FileUpdate
--     -> SyncState
--     -> SyncState
-- updateSync FU{..} SS{..} =

-- data Update = FileChange FilePath UTCTime BS.ByteString
--             | FileAdd    FilePath UTCTime BS.ByteString

