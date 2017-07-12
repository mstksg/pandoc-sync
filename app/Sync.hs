{-# LANGUAGE OverloadedLists #-}

import           Control.Lens
import           Control.Monad
import           Data.Default
import           Text.Pandoc.Sync

sc :: SyncConfig
sc = SC (DMParallelTree ext)
        fms
        "test-sync"
        ".pandoc-sync"
  where
    ext = [("markdown", "md"  )
          ,("plain"   , "txt" )
          ,("html"    , "html")
          ,("pdf"     , "pdf" )
          ,("latex"   , "tex" )
          ]
    fms = [("md"  , Writer $ FormatOptions (FMarkdown MDPandoc) def def)
          ,("txt" , Writer $ FormatOptions FPlain               def def)
          ,("html", Writer $ FormatOptions (FHTML True)         def def)
          ,("pdf" , Writer $ FormatOptions (FPDF PTLaTeX)       def def)
          ,("tex" , Writer $ FormatOptions FLaTeX               def def)
          ]

main :: IO ()
main = withSync_ sc runSync

