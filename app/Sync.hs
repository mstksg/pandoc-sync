{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Foldable
import           Data.Monoid
import           Data.Yaml           (decodeFileEither, encode, prettyPrintParseException, encodeFile)
import           Options.Applicative
import           System.Directory
import           System.Exit
import           System.FSNotify
import           System.IO.Error
import           System.Log.Logger
import           Text.Pandoc.Sync
import           Text.Printf
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T

data Command = CGenConfig  { _force  :: Bool }
             | CSync       { _dryRun :: Bool }
             | CClean
             | CWatch

data Opts = O { oConfigFile   :: FilePath
              , oLogLevel     :: Priority
              , oConflictMode :: ConflictMode
              , oCommand      :: Command
              }

parseOpts :: Parser (Command -> Opts)
parseOpts =
    O <$> strOption ( long "config"
                   <> short 'c'
                   <> metavar "FILE"
                   <> help "Config file"
                   <> value "pandoc-sync.yaml"
                   <> showDefaultWith id
                    )
      <*> asum [ flagOf DEBUG   "verbose" (Just 'v') "Verbose logging"
               , flagOf WARNING "silent"  (Just 's') "Silent non-error output"
               , pure NOTICE
               ]
      <*> asum [ flagOf CMInteractive "interactive" (Just 'i') "Solve conflicts interactively"
               , flagOf CMOldest      "oldest"      (Just 'o') "Chose oldest file to resolve conflict"
               , flagOf CMIgnore      "ignore-conflicts" Nothing "Ignore all conflicts"
               , pure CMNewest
               ]
  where
    flagOf :: a -> String -> Maybe Char -> String -> Parser a
    flagOf x n s h = flag' x (long n <> maybe mempty short s <> help h )

parseCommand :: Parser Command
parseCommand = subparser . mconcat $
    [ command "sync"       $
        info ((CSync <$> switch
                ( long "dry-run"
               <> short 'd'
               <> help "Do not write any files, and output what would be written to"
                )
              ) <**> helper
             )
             (progDesc "Update files to maintain synchronization")
    , command "watch"      $
        info (pure CWatch)
             (progDesc "Watch file directory for changes, and keep files in sync")
    , command "clean"      $
        info (pure CClean)
             (progDesc "Clean cache and reset state")
    , command "gen-config" $
        info ((CGenConfig . not <$> switch
                ( long "force"
               <> short 'f'
               <> help "Overwrite existing config file if present."
                )
              ) <**> helper
             )
             (progDesc "Generate sample configuration")
    ]

main :: IO ()
main = do
    O{..} <- execParser $ info ((parseOpts <*> parseCommand) <**> helper)
        ( fullDesc
       <> progDesc "Keep files of different formats in sync using pandoc"
       <> header "pandoc-sync - Sync different formats"
        )
    updateGlobalLogger "pandoc-sync" (setLevel oLogLevel)

    case oCommand of
      CClean -> do
        sc <- loadConfig oConfigFile
        noticeM "pandoc-sync" $ "Resetting cache at " ++ (sc ^. scCache)
        catch (removeFile (sc ^. scCache)) $ \e ->
          unless (isDoesNotExistError e) $ throw e
      CGenConfig force -> do
        exists <- doesFileExist oConfigFile
        case (exists, force) of
          (True , False) ->
            errorM "pandoc-sync" $
              printf "Could not generate sample configuration at %s (file already exists)"
                     oConfigFile
          (True , True) -> do
            encodeFile oConfigFile sampleConfig
            warningM "pandoc-sync" $
              printf "Overwriting file at %s with sample configuration file (--force applied)"
                     oConfigFile
          (False, _   ) -> do
            encodeFile oConfigFile sampleConfig
            noticeM "pandoc-sync" $
              printf "Generated sample config file to %s" oConfigFile
      CSync dry -> do
        sc <- loadConfig oConfigFile
        debugM "pandoc-sync" $
          printf "Running sync command%s%s"
            (if dry then " (dry run)" else "")
            (case oConflictMode of CMInteractive -> " (interactive)"
                                   _             -> ""
            )
        withSync_ sc $ runSync dry oConflictMode
      CWatch -> withManager $ \mgr -> do
        sc <- loadConfig oConfigFile
        new <- newMVar True
        debugM "pandoc-sync" $ printf "Watching for file changes in directory %s" (sc ^. scRoot)
        _ <- watchTree mgr (sc ^. scRoot) (const True) $ \e -> do
          infoM "pandoc-sync" "Change detected!"
          debugM "pandoc-sync" $ show e
          modifyMVar_ new $ \_ -> return True
        watchThread oConflictMode sc new
  where
    loadConfig :: FilePath -> IO SyncConfig
    loadConfig fp = do
      sce <- decodeFileEither fp
      case sce of
        Left e -> do
          errorM "pandoc-sync" "Could not parse log file:"
          errorM "pandoc-sync" (prettyPrintParseException e)
          exitFailure
        Right sc -> do
          debugM "pandoc-sync" $ printf "Loaded configuration file at %s" fp
          debugM "pandoc-sync" . T.unpack . T.decodeUtf8 $ encode sc
          return sc

watchThread :: ConflictMode -> SyncConfig -> MVar Bool -> IO ()
watchThread cm sc new = forever $ do
    updated <- modifyMVar new $ \case
      True  -> return (False, True )
      False -> return (False, False)
    when updated $ do
      infoM "pandoc-sync" "File change detected, re-syncing ..."
      withSync_ sc $ runSync False  cm
    threadDelay 1000000


sampleConfig :: SyncConfig
sampleConfig =
    SC (DMParallelTree $ M.fromList [("src","md")
                                    ,("out-pdf","pdf")
                                    ,("out-html","html")
                                    ]
       )
       (M.fromList
          [("md"  , Writer $ FormatOptions (FMarkdown MDPandoc False) def def)
          ,("pdf" , Writer $ FormatOptions (FPDF PTLaTeX            ) def def)
          ,("html", Writer $ FormatOptions (FHTML True              ) def def)
          ]
       )
       "files"
       ".pandoc-sync-cache"
