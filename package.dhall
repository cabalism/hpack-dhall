{ name =
    "hpack-dhall"
, version =
    "0.3.0"
, author =
    "Simon Hengel <sol@typeful.net>"
, maintainer =
    "Phil de Joux <phil.dejoux@blockscope.com>"
, copyright =
    "\u00A9 2017-2018 Simon Hengel, \u00A9 2018 Phil de Joux"
, license =
    "MPL-2.0"
, license-file =
    "LICENSE.md"
, category =
    "Development"
, synopsis =
    "Dhall support for Hpack"
, description =
    "This package allows you to use the Dhall configuration language to specify Haskell packages."
, github =
    "blockscope/hpack-dhall"
, tested-with =
    "GHC == 8.2.2"
, extra-source-files =
    [ "package.dhall", "changelog.md", "README.md" ]
, ghc-options =
    "-Wall"
, dependencies =
    [ "base == 4.*"
    , "megaparsec"
    , "dhall"
    , "dhall-json"
    , "hpack >= 0.26.0"
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
