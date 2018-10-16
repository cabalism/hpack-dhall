{ name =
    "hpack-dhall"
, version =
    "0.3.0"
, maintainer =
    "Simon Hengel <sol@typeful.net>"
, license =
    "PublicDomain"
, category =
    "Development"
, synopsis =
    "Dhall support for Hpack"
, description =
    "This package allows you to use the Dhall configuration language to specify Haskell packages."
, github =
    "sol/hpack-dhall"
, tested-with =
    "GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.3"
, ghc-options =
    "-Wall"
, dependencies =
    [ "base == 4.*"
    , "megaparsec >= 7.0.1"
    , "dhall >= 1.18.0"
    , "dhall-json >= 1.2.4"
    , "hpack >= 0.31.0"
    , "transformers"
    , "aeson"
    , "text"
    ]
, source-dirs =
    "src"
, executable =
    { main = "Main.hs", source-dirs = "driver" }
, tests =
    { spec =
        { main =
            "Spec.hs"
        , source-dirs =
            "test"
        , dependencies =
            [ "hspec == 2.*", "mockery", "interpolate" ]
        }
    }
}
