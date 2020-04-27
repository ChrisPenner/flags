{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut"
  , "console"
  , "effect"
  , "node-fs"
  , "node-fs-aff"
  , "node-process"
  , "node-streams"
  , "psci-support"
  , "simple-json"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}