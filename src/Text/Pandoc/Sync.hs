{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

module Text.Pandoc.Sync (
    Config
  , updateCanonical
  , syncBranch
  , updateBranch
  , pushBranch
  , pullBranch
  ) where

-- import           Control.Alternative.Free.Final
-- import           Data.List
-- import           System.Directory
-- import qualified Data.Aeson.Types               as A
-- import qualified Data.ByteString                as BS
-- import qualified Data.Text                      as T
-- import qualified Generics.SOP                   as SOP
import           Control.Applicative.Free
import           Control.Lens
import           Control.Monad.Operational
import           Data.Aeson
import           Data.Default
import           Data.Diff
import           Data.Foldable
import           Data.Kind
import           Data.Maybe
import           Data.Semigroup hiding        (diff)
import           Data.Time
-- import           Data.Type.Disjunction
import           Orphans                      ()
import           System.FilePath
import           Text.Pandoc.Sync.Format
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map                     as M
import qualified Data.Map.Merge.Lazy          as M
import qualified Data.Set                     as S
import qualified Text.Pandoc.Definition       as P

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
    mempty  = O
    mappend = (<>)

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
      return $ FC opts (collapseMap (\p f -> BS p f "md") branches)

instance FromJSON Opts where
    parseJSON = withObject "Opts" $ \v ->
      pure O

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> do
        opts     <- v .:? "options"  .!= def
        branches <- v .:? "branches" .!= M.empty
        files    <- v .:? "files"    .!= M.empty
        return $ C opts (collapseMap (\p f -> BS p f "md") branches) files

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

data AskFileF :: Type -> Type -> Type where
    AF :: FilePath -> AskFileF t t

type AskFile t = Ap (AskFileF t)

askFile :: FilePath -> AskFile t t
askFile = liftAp . AF

data UpdateFileF :: Type -> Type -> Type where
    UF :: FilePath -> s -> UpdateFileF s ()

-- data TellFileF :: Type -> Type -> Type where
--     TF :: FilePath -> s -> TellFileF s ()

-- type TellFile t = Ap (TellFileF t)

-- tellFile :: FilePath -> t -> TellFile t ()
-- tellFile fp = liftAp . TF fp

-- type TellFile t = Ap (AskFileF t :|: TellFileF t)

-- tellFile :: FilePath -> t -> TellFile t ()
-- tellFile fp = liftAp . R . TF fp

-- askFile' :: FilePath -> TellFile t t
-- askFile' = liftAp . L . AF

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


updateBranch
    :: CanPath
    -> UTCTime
    -> Branch
    -> Branch
updateBranch p t = set (at p) (Just t)

updateCanonical
    :: M.Map BranchSpec Branch
    -> Canonical
    -> AskFile P.Pandoc Canonical
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
        -> AskFile P.Pandoc (Maybe CFile)
    mkUpdate p pd0 us = do
        res <- fmap (mergeDocs pd0)
             . traverse (askFile . flip realizedPath p)
             . foldMap toList
             $ us
        pure $ CF res . fst <$> M.lookupMax us

allRequests
    :: AskFile a b
    -> S.Set FilePath
allRequests = getConst . runAp (\case AF fp -> Const (S.singleton fp))

syncBranch
    :: Canonical
    -> Branch
    -> Branch
syncBranch cs = M.merge (M.mapMissing $ \_ (CF _ t) -> t)
                        M.dropMissing
                        (M.zipWithMatched $ \_ (CF _ t0) t1 -> max t0 t1)
                        cs

pullBranch
    :: BranchSpec
    -> S.Set CanPath
    -> AskFile UTCTime Branch
pullBranch bs = sequenceA . M.fromSet (askFile . realizedPath bs)

pushBranch
    :: BranchSpec
    -> Canonical
    -> M.Map FilePath P.Pandoc  -- ^ list of files to update
pushBranch bs = M.fromList
              . map (bimap (realizedPath bs) _cfPandoc)
              . M.toList

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

-- syncSS
--     :: SyncState
--     -> Program (AskFileF P.Pandoc :|: AskFileF UTCTime :|: UpdateFileF P.Pandoc) SyncState
-- syncSS = undefined
-- -- data SyncState = SS { _ssCanonical :: Canonical
-- --                     , _ssBranches  :: M.Map BranchSpec Branch
-- --                     }


-- updateSync
--     :: FileUpdate
--     -> SyncState
--     -> (SyncState, [Int])
-- updateSync FU{..} SS{..} = _

-- data Update = FileChange FilePath UTCTime BS.ByteString
--             | FileAdd    FilePath UTCTime BS.ByteString
