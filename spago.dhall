{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-vdom"
, dependencies =
  [ "bifunctors"
  , "console"
  , "effect"
  , "exists"
  , "foreign"
  , "foreign-object"
  , "js-timers"
  , "maybe"
  , "nullable"
  , "prelude"
  , "psci-support"
  , "refs"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
