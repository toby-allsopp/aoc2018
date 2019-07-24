{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "day19"
, dependencies =
    [ "arrays"
    , "console"
    , "control"
    , "effect"
    , "psci-support"
    , "strings"
    , "unordered-collections"
    , "unicode"
    ]
, packages =
    ./packages.dhall
}
