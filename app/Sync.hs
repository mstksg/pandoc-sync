{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Data.Yaml hiding               (Parser)
import           Options.Applicative
import           Options.Applicative.Help.Chunk
import           System.Directory
import           System.Exit
import           System.FSNotify
import           System.FilePath
import           System.IO
import           System.IO.Error
import           System.Log.Formatter
import           System.Log.Handler.Simple
import           System.Log.Logger
import           Text.Pandoc.Sync
import           Text.PrettyPrint.ANSI.Leijen   (indent)
import           Text.Printf
import qualified Data.Map                       as M
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified System.Log.Handler             as H

data Command = CGenConfig  { _force  :: Bool }
             | CSync       { _dryRun :: Bool }
             | CClean
             | CWatch
  deriving Show

data Opts = O { oConfigFile   :: FilePath
              , oLogFile      :: Maybe FilePath
              , oLogLevel     :: Priority
              , oConflictMode :: Maybe ConflictMode
              , oInitMode     :: Maybe ConflictMode
              , oInteractive  :: Bool
              , oCommand      :: Command
              }
  deriving Show

parseOpts :: Parser (Command -> Opts)
parseOpts =
    O <$> strOption ( long "config"
                   <> short 'c'
                   <> metavar "FILE"
                   <> help "Config file"
                   <> value "pandoc-sync.yaml"
                   <> showDefaultWith id
                    )
      <*> optional (
            strOption ( long "log"
                     <> short 'l'
                     <> metavar "FILE"
                     <> help "Write event log to file"
                      )
         )
      <*> asum [ flagOf INFO    "verbose" (Just 'v') "Verbose logging"
               , flagOf DEBUG   "debug"   Nothing    "Log debug statements"
               , flagOf WARNING "silent"  (Just 's') "Silent non-error output"
               , pure NOTICE
               ]
      <*> optional (
            option readCM ( long "conflict"
                         <> metavar "METHOD"
                         <> help "Conflict resolution method if more than one file has been modified. (default: oldest)"
                          )
          )
      <*> optional (
            option readCM ( long "init"
                         <> metavar "METHOD"
                         <> help (unwords ["Method to chose which file to keep when new file is discovered."
                                          ,"By default wil be the same as --conflict if given, otherwise is"
                                          ,"interactive."
                                          ]
                                 )
                          )
          )
      <*> switch ( long "interactive"
                <> short 'i'
                <> help "Solve all conflicts interactively. (Overrides --conflict and --init)"
                 )
  where
    flagOf :: a -> String -> Maybe Char -> String -> Parser a
    flagOf x n s h = flag' x (long n <> maybe mempty short s <> help h )
    readCM :: ReadM ConflictMode
    readCM = maybeReader (decode . T.encodeUtf8 . T.pack)
      -- "interactive" -> Just CMInteractive
      -- "oldest"      -> Just CMOldest

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
    removeAllHandlers

    o@O{..} <- execParser $ info ((parseOpts <*> parseCommand) <**> helper)
        ( fullDesc
       <> header "pandoc-sync - Sync different formats"
       <> progDescDoc (Just description)
        )

    let defConflict | oInteractive = CMInteractive
                    | otherwise    = CMOldest
        defInit     = CMInteractive
        -- defInit     | oInteractive = CMInteractive
        --             | otherwise    = case oCommand of
        --   CWatch -> CMIgnore
        --   _      -> CMInteractive
        conflictMode = fromMaybe defConflict oConflictMode
        initMode     = fromMaybe defInit   $ oInitMode <|> oConflictMode


    hFile   <- forM oLogFile $ \fp ->
      fileHandler fp DEBUG <&> \lh ->
        H.setFormatter lh (tfLogFormatter "%F %X" "$time [$prio] $msg")
    hStream <- streamHandler stderr DEBUG <&> \lh ->
        H.setFormatter lh (tfLogFormatter "%F %X" "[$prio] $msg")

    updateGlobalLogger "pandoc-sync" $ setLevel oLogLevel
                                     . setHandlers (hStream : maybeToList hFile)

    forM_ oLogFile $ \fp -> do
      noticeM "pandoc-sync" $ printf "pandoc-sync invoked, logging to %s" fp
      debugM "pandoc-sync" (show o)

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
            (case conflictMode of CMInteractive -> " (interactive)"
                                  _             -> ""
            )
        withSync_ sc $ runSync dry initMode conflictMode
      CWatch -> withManager $ \mgr -> do
        when (oLogLevel >= NOTICE) $
          updateGlobalLogger "pandoc-sync.negative" (setLevel WARNING)
        sc <- loadConfig oConfigFile
        new <- newMVar True
        debugM "pandoc-sync" $ printf "Watching for file changes in directory %s" (sc ^. scRoot)
        let isNotCache fp = takeFileName fp /= takeFileName (sc ^. scCache)
        _ <- watchTree mgr (sc ^. scRoot) (isNotCache . eventPath) $ \e -> do
          debugM "pandoc-sync" "fsnotify event received"
          debugM "pandoc-sync" $ show e
          modifyMVar_ new $ \_ -> return True
        watchThread initMode conflictMode sc new
  where
    loadConfig :: FilePath -> IO SyncConfig
    loadConfig fp = do
      sce <- decodeFileEither fp
      case sce of
        Left e -> do
          errorM "pandoc-sync" "Could not parse config file:"
          errorM "pandoc-sync" (prettyPrintParseException e)
          exitFailure
        Right sc -> do
          debugM "pandoc-sync" $ printf "Loaded configuration file at %s" fp
          debugM "pandoc-sync" . T.unpack . T.decodeUtf8 $ encode sc
          return sc
    description = extractChunk . vsepChunks $
      [ paragraph "Keeps files of different formats in sync using pandoc."
      , vcatChunks [ paragraph "Methods for conflict resolution include:"
                   , fmap (indent 2) . vcatChunks $
                       [ paragraph "interactive: Prompt user for manual choice on file to keep"
                       , paragraph "newest: Keep last-modified file"
                       , paragraph "oldest: Keep first-modified file out of changed files"
                       , paragraph "ignore: Do not sync files where a conflict exists"
                       , paragraph "error: Halt sync immediately when conflict is found"
                       ]
                   ]
      ]

watchThread :: ConflictMode -> ConflictMode -> SyncConfig -> MVar Bool -> IO ()
watchThread cm0 cm1 sc new = forever $ do
    updated <- modifyMVar new $ \case
      True  -> return (False, True )
      False -> return (False, False)
    when updated $ do
      infoM "pandoc-sync" "File change detected, re-syncing ..."
      withSync_ sc $ runSync False cm0 cm1
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
       "./"
       ".pandoc-sync-cache"
