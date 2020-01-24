{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "frontend"
, dependencies =
    [ "affjax"
    , "assert"
    , "canvas"
    , "console"
    , "datetime"
    , "effect"
    , "foreign-generic"
    , "formatters"
    , "halogen"
    , "halogen-bootstrap4"
    , "halogen-css"
    , "halogen-formless"
    , "js-timers"
    , "now"
    , "numbers"
    , "parsing"
    , "prelude"
    , "psci-support"
    , "routing"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
