{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-halogen-mado"
, dependencies =
    [ "halogen"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
