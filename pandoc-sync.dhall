let prelude = ./dhall/prelude.dhall

let types = ./dhall/types.dhall

let branch = prelude.defaultBranch

let Format = types.Format

in  prelude.defaultConfig
    [ branch "md" [ Format.Markdown {=} ]
    , branch "md/md2" [ Format.Markdown {=} ]
    , branch "doc" [ Format.Docx {=}, Format.Rtf {=} ]
    , branch "web" [ Format.HTML {=} ]
    ]
