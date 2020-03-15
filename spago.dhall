{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-halogen-mado"
, dependencies =
    [ "console"
    , "effect"
    , "halogen"
    , "psci-support"
    , "routing"
    , "routing-duplex"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
