{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "profunctor-lenses"
    , "psci-support"
    , "spec"
    , "spec-discovery"
    , "transformers"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
