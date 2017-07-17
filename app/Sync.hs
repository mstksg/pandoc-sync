{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Lens
import           Control.Monad
import           Control.Monad.Logger
import           Data.Default
import           Data.Yaml
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
    sce <- decodeFileEither "pandoc-sync.yaml"
    case sce of
      Left  e  -> print e
      Right sc -> do
        print sc
        runStderrLoggingT $ withSync_ sc runSync

