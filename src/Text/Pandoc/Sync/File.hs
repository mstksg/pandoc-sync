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
import           Control.Monad.Logger
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Crypto.Hash.MD5              (hashlazy)
import           Data.Binary.Orphans          ()
import           Data.Dependent.Sum
import           Data.Foldable
import           Data.Kind
import           Data.Monoid
import           Data.Singletons
import           Data.Singletons.Prelude.Bool
import           Data.Time.Clock
import           Data.Witherable
import           GHC.Generics                 (Generic)
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           Text.Pandoc.Sync.Format      as PS
import           Text.Pandoc.Sync.Writer
import           Text.Printf
import qualified Data.Binary                  as Bi
import qualified Data.ByteString.Base16.Lazy  as B16
import qualified Data.ByteString.Lazy         as B
import qualified Data.Map                     as M
import qualified Data.OrdPSQ                  as PS
import qualified Text.Pandoc                  as P
import qualified Text.Pandoc.MediaBag         as P
import qualified Text.Pandoc.Readers.LaTeX    as P
import qualified Text.Pandoc.Shared           as P

data Snapshot = Snap { _snapTime :: UTCTime
                     , _snapHash :: B.ByteString
                     }
  deriving (Generic, Show, Eq, Ord)

makeLenses ''Snapshot

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
    -- , _sfLastUpdate :: Maybe (UTCTime, P.Pandoc, P.MediaBag)
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

runSyncFile :: forall m. MonadLoggerIO m => SyncFile -> m SyncFile
runSyncFile s0 = do
    syncTime <- liftIO $ getCurrentTime
    -- putStrLn $ "Last mod time: " ++ show (sfd ^. sfdLastSync)
    (updatesE, updates) <- fmap (M.mapEither id . catMaybes)
             . ifor (s0 ^. sfSources) $ \fp sfd -> runMaybeT $ do
      newSnap <- MaybeT . liftIO
                           . fmap (either (const Nothing) Just)
                           . tryJust (guard . isDoesNotExistError)
                           $
         Snap <$> getModificationTime fp <*> hashFile fp
    -- s <- tryJust (guard . isDoesNotExistError) $ Bi.decodeFile (sc ^. scCache)
      mapM_ (guard . (`snapUpdated` newSnap)) (sfd ^. sfdLastSync)
      mapM_ (guard . (`snapUpdated` newSnap)) (s0  ^? sfLastUpdate . _Just . _1)
      -- liftIO $ printf "Found updated source: %s (%s -> %s)\n"
      --                 fp
      --                 (show (sfd ^. sfdLastSync))
      --                 (show modTime)
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
    let sortedUpdates = mapToQueue id updates
    case PS.minView sortedUpdates of
      Nothing -> do
        liftIO $ putStrLn "No updates found"
        case s0 ^? sfLastUpdate . _Just . to (\(_,pd,mb) -> (pd, mb)) of
          Nothing -> return s0
          Just (pd, mb) -> do
            liftIO $ putStrLn "Updating new sinks anyway"
            s0 & sfSinks . itraversed %%@~ \fp' sfd -> do
              exists <- liftIO $ doesFileExist fp'
              updateSource syncTime
                           (has (sfdLastSync . _Just) sfd && exists)
                           pd mb fp' sfd
      Just (fp, _, (pd, mb), laterUpdates) -> do
        liftIO $ do
          putStrLn "Updates found!"
          putStrLn $ "Last update was " ++ show (s0 ^? sfLastUpdate . _Just . _1)
          putStrLn $ "Using update from: " ++ fp
        itraverse_ backupError updatesE
        itraverse_ backupLater $ queueToMap (flip const) laterUpdates
        let pdHash = B16.encode . B.fromStrict . hashlazy $ Bi.encode pd <> Bi.encode mb
        s0 & sfLastUpdate                  .~ Just (Snap syncTime pdHash, pd, mb)
           & sfSourcesSinks . itraversed %%@~ \fp' (s :=> sfd) -> withSingI s $ do
               sfd' <- updateSource syncTime (fp == fp') pd mb fp' sfd
               return $ s :=> sfd'
  where
    backupError :: FilePath -> P.PandocError -> m ()
    backupError fp _ = liftIO . putStrLn $ "Error reading " ++ fp ++ " !"
    backupLater :: FilePath -> (P.Pandoc, P.MediaBag) -> m ()
    backupLater fp _ = liftIO . putStrLn $ "Update conflit! " ++ fp
    updateSource
        :: SingI r
        => UTCTime
        -> Bool
        -> P.Pandoc
        -> P.MediaBag
        -> FilePath
        -> SyncFileData r
        -> m (SyncFileData r)
    updateSource st skipWrite pd mb fp sfd = liftIO $ do
      createDirectoryIfMissing True (takeDirectory fp)
      unless skipWrite . liftIO $ do
        putStrLn $ "Updating " ++ fp
        writePandoc (sfd ^. sfdFormat) pd mb (sfd ^. sfdWriterOpts) fp
      hsh <- catch (hashFile fp) (\e -> putStrLn "goodbye" *> throwIO @SomeException e)
      return $ sfd & sfdLastSync .~ Just (Snap st hsh)

hashFile :: FilePath -> IO B.ByteString
hashFile fp = B16.encode . B.fromStrict . hashlazy <$> B.readFile fp

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
