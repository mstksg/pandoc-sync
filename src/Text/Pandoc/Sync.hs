{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeInType          #-}

module Text.Pandoc.Sync (
    Config
  , updateCanonical
  , syncBranches
  , updateBranch
  ) where

-- import           Control.Alternative.Free.Final
-- import           Control.Applicative.Free
-- import           Data.List
-- import qualified Data.Aeson.Types               as A
-- import qualified Data.ByteString                as BS
-- import qualified Data.Text                      as T
import           Control.Lens
import           Control.Monad.Operational
import           Data.Aeson
import           Data.Default
import           Data.Diff
import           Data.Foldable
import           Data.Kind
import           Data.Maybe
import           Data.Semigroup hiding             (diff)
import           Data.Time
import           System.FilePath
import           Text.Pandoc.Sync.Format
import qualified Data.List.NonEmpty                as NE
import qualified Data.Map                          as M
import qualified Data.Map.Merge.Lazy               as M
import qualified Data.Set                          as S
import qualified Generics.SOP                      as SOP
import qualified Text.Pandoc.Definition            as P

type CanPath = FilePath
type Extension = String

data BranchSpec = BS { _bsRoot   :: FilePath
                     , _bsFormat :: SomeFormat
                     , _bsExt    :: Extension
                     }
  deriving (Show, Eq, Ord)
makeLenses ''BranchSpec

data CFile = CF { _cfPandoc :: P.Pandoc
                , _cfUpdate :: UTCTime
                }
  deriving (Show, Eq, Ord)

makeLenses ''CFile

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
      return $ FC opts (collapseMap (\p (f, e) -> BS p f e) branches)

instance FromJSON Opts where
    parseJSON = withObject "Opts" $ \v ->
      pure O

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> do
        opts     <- v .:? "options"  .!= def
        branches <- v .:? "branches" .!= M.empty
        files    <- v .:? "files"    .!= M.empty
        return $ C opts (collapseMap (\p (f, e) -> BS p f e) branches) files

-- classify
--     :: Config
--     -> FilePath
--     -> M.Map BranchSpec CanPath     -- canonical
-- classify C{..} fp = M.fromList $ do
--     rootElems <- drop 1 . reverse . inits . splitPath $ fp
--     let root = takeDirectory . joinPath $ rootElems
--         bs   = BS root format
--         can  = dropExtension . makeRelative root $ fp
--     _ <- maybeToList $ M.lookup bs _cBranches
--     return (bs, can)
--   where
--     format = inferFormat (drop 1 (takeExtension fp))

data LoadPF :: Type -> Type where
    LoadP :: FilePath -> LoadPF P.Pandoc

type LoadP = Program LoadPF

loadP :: FilePath -> LoadP P.Pandoc
loadP = singleton . LoadP

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

realizedPath
    :: BranchSpec
    -> CanPath
    -> FilePath
realizedPath BS{..} p = _bsRoot </> p -<.> _bsExt

updateBranch
    :: CanPath
    -> UTCTime
    -> Branch
    -> Branch
updateBranch p t = set (at p) (Just t)

updateCanonical
    :: M.Map BranchSpec Branch
    -> Canonical
    -> LoadP Canonical
updateCanonical bs = M.mergeA (M.traverseMaybeMissing $ \p -> mkUpdate p mempty)
                              M.preserveMissing
                              (M.zipWithMaybeAMatched $ \p us (CF pd0 t0) ->
                                    let (_, candidates) = M.split t0 us
                                    in  mkUpdate p pd0 candidates
                              )
                              paths
  where
    -- | Map of all canonical paths inside 'bs', sorted by update time
    paths :: M.Map CanPath (M.Map UTCTime (S.Set BranchSpec))
    paths = (M.unionsWith . M.unionWith) S.union
          . ifoldMap (\sp -> (:[]) . fmap (flip M.singleton (S.singleton sp)))
          $ bs
    mkUpdate
        :: CanPath
        -> P.Pandoc
        -> M.Map UTCTime (S.Set BranchSpec)
        -> LoadP (Maybe CFile)
    mkUpdate p pd0 us = do
        res <- fmap (mergeDocs pd0)
             . traverse (loadP . flip realizedPath p)
             . foldMap toList
             $ us
        pure $ CF res . fst <$> M.lookupMax us

syncBranches
    :: Canonical
    -> M.Map BranchSpec Branch
    -> M.Map BranchSpec Branch
syncBranches cs = fmap go
  where
    go :: Branch -> Branch
    go = M.merge (M.mapMissing $ \_ (CF _ t) -> t)
                 M.dropMissing
                 (M.zipWithMatched $ \_ (CF _ t0) t1 -> max t0 t1)
                 cs

-- type Branch    = M.Map CanPath UTCTime

mergeDocs :: P.Pandoc -> [P.Pandoc] -> P.Pandoc
mergeDocs orig ps = fromMaybe orig $ do
    e NE.:| es <- fmap (diff orig) <$> NE.nonEmpty ps
    patch (foldl' go e es) orig
  where
    go :: Edit P.Pandoc -> Edit P.Pandoc -> Edit P.Pandoc
    go old new = case mergePatch old new of
      Incompatible -> old
      Conflict p   -> p
      NoConflict p -> p

-- updateSync
--     :: FileUpdate
--     -> SyncState
--     -> (SyncState, [Int])
-- updateSync FU{..} SS{..} = _

-- data Update = FileChange FilePath UTCTime BS.ByteString
--             | FileAdd    FilePath UTCTime BS.ByteString

instance SOP.Generic P.Pandoc
instance SOP.Generic P.Meta
instance SOP.Generic P.MetaValue
instance SOP.Generic P.Block
instance SOP.Generic P.Inline
instance SOP.Generic P.Format
instance SOP.Generic P.QuoteType
instance SOP.Generic P.ListNumberStyle
instance SOP.Generic P.ListNumberDelim
instance SOP.Generic P.Alignment
instance SOP.Generic P.MathType
instance SOP.Generic P.Citation
instance SOP.Generic P.CitationMode

instance SOP.HasDatatypeInfo P.Pandoc
instance SOP.HasDatatypeInfo P.Meta
instance SOP.HasDatatypeInfo P.MetaValue
instance SOP.HasDatatypeInfo P.Block
instance SOP.HasDatatypeInfo P.Inline
instance SOP.HasDatatypeInfo P.Format
instance SOP.HasDatatypeInfo P.QuoteType
instance SOP.HasDatatypeInfo P.ListNumberStyle
instance SOP.HasDatatypeInfo P.ListNumberDelim
instance SOP.HasDatatypeInfo P.Alignment
instance SOP.HasDatatypeInfo P.MathType
instance SOP.HasDatatypeInfo P.Citation
instance SOP.HasDatatypeInfo P.CitationMode

instance Diff P.Pandoc
instance Diff P.Meta
instance Diff P.MetaValue
instance Diff P.Block
instance Diff P.Inline
instance Diff P.Format
instance Diff P.QuoteType
instance Diff P.ListNumberStyle
instance Diff P.ListNumberDelim
instance Diff P.Alignment
instance Diff P.MathType
instance Diff P.Citation
instance Diff P.CitationMode
