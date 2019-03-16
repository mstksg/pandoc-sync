let prelude = ./dhall/prelude.dhall

let types = ./dhall/types.dhall

let branch = prelude.defaultBranch

let Format = types.Format

in  prelude.defaultConfig
    [ branch "md" [ prelude.markdown ]
    , branch "md/md2" [ prelude.markdown ]
    , branch "doc" [ Format.DocX {=}, Format.RTF {=} ]
    , branch "web" [ Format.HTML True ]
    ]
