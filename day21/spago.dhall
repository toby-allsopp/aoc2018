{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "arrays"
    , "control"
    , "effect"
    , "console"
    , "strings"
    , "transformers"
    , "unicode"
    , "unordered-collections"
    , "psci-support" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
