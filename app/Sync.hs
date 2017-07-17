{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Yaml
import           System.Log.Logger
import           Text.Pandoc.Sync

-- sc :: SyncConfig
-- sc = SC (DMParallelTree ext)
--         fms
--         "test-sync"
--         ".pandoc-syncfile"
--   where
--     ext = [("markdown", "md"  )
--           ,("plain"   , "txt" )
--           ,("html"    , "html")
--           ,("pdf"     , "pdf" )
--           ,("latex"   , "tex" )
--           ]
--     fms = [("md"  , Writer $ FormatOptions (FMarkdown MDPandoc) def def)
--           ,("txt" , Writer $ FormatOptions FPlain               def def)
--           ,("html", Writer $ FormatOptions (FHTML True)         def def)
--           ,("pdf" , Writer $ FormatOptions (FPDF PTLaTeX)       def def)
--           ,("tex" , Writer $ FormatOptions FLaTeX               def def)
--           ]

main :: IO ()
main = do
    sce <- decodeFileEither @SyncConfig "pandoc-sync.yaml"
    case sce of
      Left  e  -> print e
      Right sc -> do
        updateGlobalLogger "pandoc-sync" (setLevel DEBUG)
        print sc
        withSync_ sc runSync

