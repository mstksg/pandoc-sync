{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Pandoc.Sync.File (
    SyncFileData(..)
  , SyncFile(..)
  , ConflictMode(..)
  , sfSourcesSinks
  , emptySyncFile
  , runSyncFile
  , module PS
  ) where

-- import qualified Data.Hashable             as Ha
-- import qualified Data.Text.Lazy.Encoding   as TL
-- import qualified Data.Text.Lazy.IO         as TL
-- import qualified Text.Pandoc.PDF           as P
-- import qualified Text.Pandoc.SelfContained as P
-- import qualified Text.Pandoc.UTF8          as UTF8
import           Control.Exception
import           Control.Lens hiding          ((<.>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Crypto.Hash.MD5              (hashlazy)
import           Data.Binary.Orphans          ()
import           Data.Dependent.Sum
import           Data.Foldable
import           Data.Kind
import           Data.Monoid
import           Data.Ord
import           Data.Singletons
import           Data.Singletons.Prelude.Bool
import           Data.Time.Clock
import           Data.Witherable
import           GHC.Generics                 (Generic)
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           System.Log.Logger
import           Text.Pandoc.Sync.Format      as PS
import           Text.Pandoc.Sync.Writer
import           Text.Printf
import           Text.Read                    (readMaybe)
import qualified Data.Binary                  as Bi
import qualified Data.ByteString.Base16.Lazy  as B16
import qualified Data.ByteString.Lazy         as B
import qualified Data.Map                     as M
import qualified Data.OrdPSQ                  as PS
import qualified Data.Set                     as S
import qualified Text.Pandoc                  as P
import qualified Text.Pandoc.MediaBag         as P
import qualified Text.Pandoc.Readers.LaTeX    as P
import qualified Text.Pandoc.Shared           as P

-- TODO: include pandoc and mediabag in snapshot
data Snapshot = Snap { _snapTime :: UTCTime
                     , _snapHash :: B.ByteString
                     }
  deriving (Generic, Show, Eq)

makeLenses ''Snapshot

instance Ord Snapshot where
    compare s1 s2 = comparing (view snapTime) s2 s1
                 <> comparing (view snapHash) s1 s2

instance Bi.Binary Snapshot

data SyncFileData :: Bool -> Type where
    SyncFileData :: { _sfdFormat     :: Format r 'True
                    , _sfdReaderOpts :: HasIf r ReaderOptions
                    , _sfdWriterOpts :: WriterOptions
                    , _sfdLastSync   :: Maybe Snapshot
                    }
                 -> SyncFileData r
  deriving (Generic, Show)

makeLenses ''SyncFileData

instance SingI r => Bi.Binary (SyncFileData r)

data SyncFile = SyncFile
    { _sfSources    :: M.Map FilePath (SyncFileData 'True )
    , _sfSinks      :: M.Map FilePath (SyncFileData 'False)
    , _sfLastUpdate :: Maybe (Snapshot, P.Pandoc, P.MediaBag)
    }
  deriving (Generic, Show)

makeLenses ''SyncFile

instance Bi.Binary P.Pandoc
instance Bi.Binary P.Meta
instance Bi.Binary P.MetaValue
instance Bi.Binary P.Inline
instance Bi.Binary P.QuoteType
instance Bi.Binary P.Citation
instance Bi.Binary P.CitationMode
instance Bi.Binary P.MathType
instance Bi.Binary P.Format
instance Bi.Binary P.Block
instance Bi.Binary P.ListNumberStyle
instance Bi.Binary P.ListNumberDelim
instance Bi.Binary P.Alignment

instance Bi.Binary SyncFile

instance Bi.Binary P.MediaBag where
    get = foldl' @[] (flip $ \(fp, mt, c) -> P.insertMedia fp (Just mt) c) mempty <$> Bi.get
    put mb = Bi.put $ mapMaybe go (P.mediaDirectory mb)
      where
        go (fp, _, _) = (\(mt, c) -> (fp, mt, c)) <$> P.lookupMedia fp mb

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

data ConflictMode = CMInteractive
                  | CMNewest
                  | CMOldest
                  | CMIgnore

runSyncFile :: ConflictMode -> SyncFile -> IO SyncFile
runSyncFile cm s0 = do
    syncTime <- getCurrentTime
    (updatesE, updates) <- fmap (M.mapEither id . catMaybes)
             . ifor (s0 ^. sfSources) $ \fp sfd -> runMaybeT $ do
      liftIO . debugM "pandoc-sync" $ printf "Checking %s for updates" fp
      newSnap <- MaybeT $ snapshot fp
      mapM_ (guard . (`snapUpdated` newSnap)) (sfd ^. sfdLastSync)
      mapM_ (guard . (`snapUpdated` newSnap)) (s0  ^? sfLastUpdate . _Just . _1)
      liftIO $ case sfd ^. sfdLastSync of
        Nothing -> noticeM "pandoc-sync" $ printf "%s updated for the first time at %s"
                     fp (show (newSnap ^. snapTime))
        Just sp -> infoM "pandoc-sync" $ printf "%s updated at %s, from previous update at %s"
                     fp (show (newSnap ^. snapTime)) (show (sp ^. snapTime))
      epd <- liftIO $ readPandoc (sfd ^. sfdFormat)
                                 (sfd ^. sfdReaderOpts . _Has . to fromReaderOptions)
                                 fp
      MaybeT . return $ case epd of
        Right (newPd, mb) -> case s0 ^? sfLastUpdate . _Just . _2 of
          Just oldPd | oldPd == newPd
                    -> Nothing
          -- _         -> Just $ Right (sfd ^. sfdLastSync, (newPd, mb))
          _         -> Just $ Right (newSnap, (newPd, mb))
        Left e      -> Just $ Left e
    -- TODO: allow the user to pick which version to use
    selected <- case cm of
      CMNewest -> return
                . over _Just (\(x, _, y, z) -> (x, y, queueToMap (flip const) z))
                . PS.minView
                . mapToQueue (over _1 (view (snapTime . _Unwrapped @(Down _))))
                $ updates
      CMOldest -> return
                . over _Just (\(x, _, y, z) -> (x, y, queueToMap (flip const) z))
                . PS.minView
                . mapToQueue (over _1 (view snapTime))
                $ updates
      CMInteractive ->
        let disp fp = printf "%s (Updated at %s)" fp (updates ^?! ix fp . _1 . snapTime . to show)
        in  over _Just (\x -> (x, snd (updates M.! x), fmap snd (M.delete x updates)))
              <$> queryUser disp "Conflict found!" (M.keysSet updates)
      CMIgnore -> do
        warningM "pandoc-sync" "Conflict found, but ignoring."
        return Nothing
    case selected of
      Nothing -> do
        noticeM "pandoc-sync" "No updates to handle"
        case s0 ^? sfLastUpdate . _Just . to (\(_,pd,mb) -> (pd, mb)) of
          Nothing -> return s0
          Just (pd, mb) -> do
            s0 & sfSinks . itraversed %%@~ \fp' sfd -> do
              snap <- snapshot fp'
              skipping <- case sfd ^. sfdLastSync of
                Nothing -> do
                  infoM "pandoc-sync" $ printf "Updating %s for the first time" fp'
                  return False
                Just snap0 -> case snap of
                  Just snap1 | not (snap0 `snapUpdated` snap1) -> do
                    debugM "pandoc-sync" $ printf "Skipping update of %s" fp'
                    return True
                  _ -> do
                    infoM "pandoc-sync" $ printf "%s desynchronized, updating ..." fp'
                    return False
              updateSource syncTime skipping pd mb fp' sfd
      Just (fp, (pd, mb), laterUpdates) -> do
        noticeM "pandoc-sync" $ printf "New update found, using %s" fp
        itraverse_ backupError updatesE
        -- itraverse_ backupLater $ queueToMap (flip const) laterUpdates
        itraverse_ backupLater laterUpdates
        let pdHash = B16.encode . B.fromStrict . hashlazy $ Bi.encode pd <> Bi.encode mb
        s0 & sfLastUpdate                  .~ Just (Snap syncTime pdHash, pd, mb)
           & sfSourcesSinks . itraversed %%@~ \fp' (s :=> sfd) -> withSingI s $ do
               sfd' <- updateSource syncTime (fp == fp') pd mb fp' sfd
               return $ s :=> sfd'
  where
    backupError :: FilePath -> P.PandocError -> IO ()
    backupError fp pe = do
      errorM "pandoc-sync" $ printf "Error reading contents of %s!" fp
      errorM "pandoc-sync" $ show pe
    backupLater :: FilePath -> (P.Pandoc, P.MediaBag) -> IO ()
    backupLater fp _ =
      errorM "pandoc-sync" $ printf "Backing up conflicting version at %s!" fp
    updateSource
        :: SingI r
        => UTCTime
        -> Bool
        -> P.Pandoc
        -> P.MediaBag
        -> FilePath
        -> SyncFileData r
        -> IO (SyncFileData r)
    updateSource st skipWrite pd mb fp sfd = do
      createDirectoryIfMissing True (takeDirectory fp)
      unless skipWrite $ do
        noticeM "pandoc-sync" $ printf "Writing to %s" fp
        writePandoc (sfd ^. sfdFormat) pd mb (sfd ^. sfdWriterOpts) fp
      hsh <- hashFile fp
      return $ sfd & sfdLastSync .~ Just (Snap st hsh)

queryUser :: forall a. (a -> String) -> String -> S.Set a -> IO (Maybe a)
queryUser f msg s | S.null s = return Nothing
                  | S.size s == 1 = return $ Just (S.elemAt 0 s)
                  | otherwise = Just <$> do
    putStrLn msg
    iforOf_ folded s $ \i a ->
      printf "%d) %s\n" (i + 1) (f a)
    putStrLn "Select choice:"
    getChoice
  where
    getChoice :: IO a
    getChoice = do
      l <- getLine
      case readMaybe l of
        Nothing ->
          putStrLn "Please enter a valid number" *> getChoice
        Just i  | i < 1  || i > S.size s ->
          putStrLn "Out of range." *> getChoice
                | otherwise ->
          return $ S.elemAt (i - 1) s


hashFile :: FilePath -> IO B.ByteString
hashFile fp = B16.encode . B.fromStrict . hashlazy <$> B.readFile fp

snapshot :: FilePath -> IO (Maybe Snapshot)
snapshot fp = fmap (either (const Nothing) Just) . tryJust (guard . isDoesNotExistError) $
    Snap <$> getModificationTime fp <*> hashFile fp

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

snapUpdated :: Snapshot -> Snapshot -> Bool
snapUpdated s0 s1 = (s0 ^. snapHash /= s1 ^. snapHash)
                 && ((s1 ^. snapTime) `diffUTCTime` (s0 ^. snapTime)) > 0.1
