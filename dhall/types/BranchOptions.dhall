{ always-backup :
    Optional Bool
, variables :
    List { key : Text, val : Text }
, discover :
    Optional Bool
, mode :
    Optional ./Mode.dhall
, priority :
    Natural
, reference :
    ./Reference.dhall
}
