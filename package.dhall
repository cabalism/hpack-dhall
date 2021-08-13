let deps =
      [ "base == 4.*"
      , "megaparsec"
      , "dhall"
      , "dhall-json"
      , "hpack >= 0.34.4"
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

in  let exe-deps =
          [ "hpack-dhall", "optparse-applicative" ]

    in  { name =
            "hpack-dhall"
        , version =
            "0.5.3"
        , maintainer =
            "Phil de Joux <phil.dejoux@blockscope.com>"
        , copyright =
            "© 2018 Phil de Joux, © 2018 Block Scope Limited"
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
            "GHC == 8.10.4"
        , extra-source-files =
            [ "package.dhall"
            , "changelog.md"
            , "README.md"
            , "test-suite-golden/**/*.dhall"
            , "test-suite-golden/**/*.dhl"
            , "test-suite-golden/**/*.cabal"
            , "test-suite-golden/**/*.json"
            , "test-suite-golden/**/*.yaml"
            , "test-suite-golden/**/*.golden"
            ]
        , ghc-options =
            [ "-Wall"
            , "-Werror"
            , "-Wincomplete-uni-patterns"
            , "-Wcompat"
            , "-Widentities"
            , "-Wredundant-constraints"
            , "-fhide-source-paths"
            ]
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
              ./default-tests.dhall
            ⫽ { golden =
                  { main =
                      "Golden.hs"
                  , source-dirs =
                      [ "test-suite-golden/src" ]
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
                      , "directory"
                      ]
                  }
              }
        }
