{
  name = "hpack-dhall"

, ghc-options = "-Wall"

, dependencies = [
    "base == 4.*"
  , "trifecta"
  , "dhall"
  , "dhall-json"
  , "hpack >= 0.23.0"
  , "transformers"
  , "aeson"
  , "text"
  ]

, source-dirs = "src"

, executable = {
    main = "Main.hs"
  , source-dirs = "driver"
  }

, tests = {
    spec = {
      main = "Spec.hs"
    , source-dirs = "test"
    , dependencies = [
        "hspec == 2.*"
      , "mockery"
      , "interpolate"
      ]
    }
  }
}
