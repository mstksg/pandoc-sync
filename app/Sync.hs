{-# LANGUAGE OverloadedLists #-}

import           Text.Pandoc.Sync
import           Control.Monad
import           Control.Lens

sc :: SyncConfig
sc = SC (DMParallelTree ext)
        fms
        "test-sync"
        ".pandoc-sync"
  where
    ext = [("markdown", "md"  )
          ,("web"     , "html")
          ]
    fms = [("md"  , Writer $ FormatOptions (FMarkdown MDPandoc) (Has RO) (Has WO))
          ,("html", Writer $ FormatOptions (FHTML True        ) (Has RO) (Has WO))
          ]

                     -- , _scDiscoverFormats :: M.Map FileExt (Writer FormatOptions)
main :: IO ()
main = withSync_ sc runSync

