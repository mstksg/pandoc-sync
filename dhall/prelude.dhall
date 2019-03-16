let types = ./types.dhall

let defaultGlobalOptions =
        { always-backup =
            False
        , discover =
            True
        , variables =
            [] : List { key : Text, val : Text }
        }
      : types.GlobalOptions

let defaultBranchOptions =
        { always-backup =
            None Bool
        , variables =
            [] : List { key : Text, val : Text }
        , discover =
            None Bool
        , mode =
            None types.Mode
        , priority =
            5
        , reference =
            types.Reference.Self {=}
        }
      : types.BranchOptions

in  { defaultGlobalOptions =
        defaultGlobalOptions
    , defaultBranchOptions =
        defaultBranchOptions
    , defaultBranch =
          λ(d : Text)
        → λ(fs : List types.Format)
        →   { dir = d, formats = fs, options = defaultBranchOptions }
          : types.Branch
    , defaultConfig =
          λ(bs : List types.Branch)
        → { options = defaultGlobalOptions, branches = bs }
    , markdown =
        types.Format.Markdown
        { type = types.MarkdownType.Pandoc {=}, lhs = False }
    }
