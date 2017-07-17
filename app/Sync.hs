{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Foldable
import           Data.Monoid
import           Data.Yaml           (decodeFileEither)
import           Options.Applicative
import           System.Directory
import           System.Log.Logger
import           Text.Pandoc.Sync

data Opts = O { oConfigFile   :: FilePath
              , oLogLevel     :: Priority
              , oClean        :: Bool
              -- _oDry
              , oConflictMode :: ConflictMode
              }

parseOpts :: Parser Opts
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
      <*> switch   ( long "clean"
                  <> help "Clean the cache"
                   )
      <*> asum [ flagOf CMInteractive "interactive" (Just 'i') "Solve conflicts interactively"
               , flagOf CMOldest      "oldest"      (Just 'o') "Chose oldest file to resolve conflict"
               , flagOf CMIgnore      "ignore-conflicts" Nothing "Ignore all conflicts"
               , pure CMNewest
               ]
  where
    flagOf :: a -> String -> Maybe Char -> String -> Parser a
    flagOf x n s h = flag' x (long n <> maybe mempty short s <> help h )

main :: IO ()
main = do
    O{..} <- execParser $ info (parseOpts <**> helper)
                            ( fullDesc
                           <> progDesc "Keep files of different formats in sync using pandoc"
                           <> header "pandoc-sync - Sync different formats"
                            )
    updateGlobalLogger "pandoc-sync" (setLevel oLogLevel)
    sce <- decodeFileEither oConfigFile
    case sce of
      Left  e  -> do
        errorM "pandoc-sync" "Could not parse log file:"
        errorM "pandoc-sync" (show e)
      Right sc -> do
        debugM "pandoc-sync" $ "Loaded configuration file at " ++ oConfigFile
        when oClean $ do
          noticeM "pandoc-sync" $ "Resetting cache at " ++ (sc ^. scCache)
          removeFile (sc ^. scCache)
        withSync_ sc $ runSync oConflictMode

