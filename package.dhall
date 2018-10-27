{ name =
    "hpack-dhall"
, version =
    "0.4.0"
, maintainer =
    "Phil de Joux <phil.dejoux@blockscope.com>"
, copyright =
    "\u00A9 2018 Phil de Joux"
, license =
    "BSD3"
, license-file =
    "LICENSE"
, category =
    "Development"
, synopsis =
    "Dhall support for Hpack"
, description =
    "This package allows you to use the Dhall configuration language to specify Haskell packages."
, github =
    "blockscope/hpack-dhall"
, tested-with =
    "GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.3, GHC == 8.6.1"
, extra-source-files =
    [ "package.dhall", "changelog.md", "README.md" ]
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
    , "microlens"
    , "filepath"
    ]
, source-dirs =
    "library"
, executable =
    { main = "Main.hs", source-dirs = "exe/hpack-dhall" }
, tests =
    { golden =
        { main =
            "Golden.hs"
        , source-dirs =
            [ "library", "test/golden" ]
        , dependencies =
            [ "base"
            , "Cabal"
            , "Diff"
            , "bytestring"
            , "dhall"
            , "filepath"
            , "microlens"
            , "prettyprinter"
            , "tasty"
            , "tasty-golden"
            , "text"
            , "megaparsec >= 7.0.1"
            , "dhall >= 1.18.0"
            , "dhall-json >= 1.2.4"
            , "hpack >= 0.31.0"
            , "transformers"
            , "aeson"
            ]
        }
    }
}
