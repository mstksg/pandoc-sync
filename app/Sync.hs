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
          ,("plain"   , "txt")
          ]
    fms = [("md" , Writer $ FormatOptions (FMarkdown MDPandoc) (Has RO) (Has WO))
          ,("txt", Writer $ FormatOptions FPlain               (Hasn't) (Has WO))
          ]

main :: IO ()
main = withSync_ sc runSync

