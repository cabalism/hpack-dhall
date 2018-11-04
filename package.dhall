    let deps =
          [ "base == 4.*"
          , "megaparsec >= 7.0.1"
          , "dhall >= 1.18.0"
          , "dhall-json >= 1.2.4"
          , "hpack >= 0.31.0"
          , "transformers"
          , "text"
          , "microlens"
          , "filepath"
          , "bytestring"
          , "prettyprinter"
          , "aeson"
          , "aeson-pretty"
          , "yaml"
          ]

in  let exe-deps = [ "hpack-dhall", "optparse-applicative" ]

in  { name =
        "hpack-dhall"
    , version =
        "0.5.0"
    , maintainer =
        "Phil de Joux <phil.dejoux@blockscope.com>"
    , copyright =
        "\u00A9 2018 Phil de Joux, \u00A9 2018 Block Scope Limited"
    , license =
        "BSD3"
    , license-file =
        "LICENSE"
    , category =
        "Development"
    , synopsis =
        "hpack's dhalling"
    , description =
        ''
        Work with hpack's top-level
        <https://github.com/sol/hpack#top-level-fields fields> in a Dhall
        record with the following executables;

        * with @dhall-hpack-cabal@ write the @.cabal@ for a @.dhall@ package description.
        * with @dhall-hpack-dhall@ show the package description expression, with imports resolved.
        * with @dhall-hpack-json@ show the package description as JSON.
        * with @dhall-hpack-yaml@ show the package description as YAML.
        ''
    , github =
        "blockscope/hpack-dhall"
    , tested-with =
        "GHC == 8.4.3, GHC == 8.4.4, GHC == 8.6.1"
    , extra-source-files =
        [ "package.dhall"
        , "changelog.md"
        , "README.md"
        , "test/golden/**/*.dhall"
        , "test/golden/**/*.dhl"
        , "test/golden/**/*.cabal"
        , "test/golden/**/*.json"
        , "test/golden/**/*.yaml"
        , "test/golden/**/*.golden"
        ]
    , ghc-options =
        "-Wall"
    , dependencies =
        deps
    , source-dirs =
        "library"
    , library =
        { exposed-modules = [ "Hpack.Dhall", "Hpack.Fields" ] }
    , executables =
        { dhall-hpack-cabal =
            { main =
                "CabalMain.hs"
            , source-dirs =
                [ "exe/options", "exe/dhall-hpack-cabal" ]
            , dependencies =
                exe-deps
            }
        , dhall-hpack-json =
            { main =
                "JsonMain.hs"
            , source-dirs =
                [ "exe/options", "exe/dhall-hpack-json" ]
            , dependencies =
                exe-deps
            }
        , dhall-hpack-yaml =
            { main =
                "YamlMain.hs"
            , source-dirs =
                [ "exe/options", "exe/dhall-hpack-yaml" ]
            , dependencies =
                exe-deps
            }
        , dhall-hpack-dhall =
            { main =
                "DhallMain.hs"
            , source-dirs =
                [ "exe/options", "exe/dhall-hpack-dhall" ]
            , dependencies =
                exe-deps
            }
        }
    , tests =
        { golden =
            { main =
                "Golden.hs"
            , source-dirs =
                [ "test/golden/src" ]
            , dependencies =
                [ "base"
                , "Cabal"
                , "Diff"
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
                , "utf8-string"
                ]
            }
        }
    }
