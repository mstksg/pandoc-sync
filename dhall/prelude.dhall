let Types = ./types.dhall

let defaultGlobalOptions =
        { always-backup =
            False
        , discover =
            True
        , variables =
            [] : List { key : Text, val : Text }
        }
      : Types.GlobalOptions

let defaultBranchOptions =
        { always-backup =
            None Bool
        , variables =
            [] : List { key : Text, val : Text }
        , discover =
            None Bool
        , mode =
            None Types.Mode
        , priority =
            5
        , reference =
            Types.Reference.Self {=}
        }
      : Types.BranchOptions

in  { defaultGlobalOptions =
        defaultGlobalOptions
    , defaultBranchOptions =
        defaultBranchOptions
    , defaultBranch =
        λ(d : Text) → { dir = d, options = defaultBranchOptions } : Types.Branch
    , defaultConfig =
        { options = defaultGlobalOptions, branches = [] : List Types.Branch }
    }
