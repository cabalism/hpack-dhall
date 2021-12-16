let deps =
      [ "base >= 4.13 && < 5"
      , "megaparsec >= 9.2.0"
      , "dhall >= 1.40.2"
      , "dhall-json >= 1.7.9"
      , "hpack >= 0.34.6"
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

in  let exe-deps = [ "optparse-applicative" ]

    in  { name = "hpack-dhall"
        , version = "0.5.4"
        , maintainer = "Phil de Joux <phil.dejoux@blockscope.com>"
        , copyright =
            "© 2018 - 2021 Phil de Joux, © 2018 - 2021 Block Scope Limited"
        , license = "BSD3"
        , license-file = "LICENSE"
        , category = "Development"
        , synopsis = "hpack's dhalling"
        , description =
            ''
            Use hpack phrasing in dhall to write cabal files.

            There are two main reasons why you'd use hpack-dhall, convenience and safety.

            Get the convenience of hpack. Don't bother to state what can be inferred or
            defaulted, easing the burden of completing a package description by hand.  For
            example `other-modules` can be inferred by taking the set difference between
            modules on disk and the set of `exposed-modules`.

            Get the safety of dhall's programmable configuration: typed fields, safe imports
            and functions.
            ''
        , github = "cabalism/hpack-dhall"
        , tested-with = "GHC == 8.8.4, GHC == 8.10.7"
        , extra-source-files =
          [ "package.dhall"
          , "changelog.md"
          , "test-suite-golden/**/*.dhall"
          , "test-suite-golden/**/*.dhl"
          , "test-suite-golden/**/*.cabal"
          , "test-suite-golden/**/*.json"
          , "test-suite-golden/**/*.yaml"
          , "test-suite-golden/**/*.golden"
          ]
        , ghc-options =
          [ "-Wall"
          , "-Wincomplete-uni-patterns"
          , "-Wcompat"
          , "-Widentities"
          , "-Wredundant-constraints"
          , "-fhide-source-paths"
          ]
        , dependencies = deps
        , source-dirs = "library"
        , library.exposed-modules = [ "Hpack.Dhall", "Hpack.Fields" ]
        , executables =
          { dhall-hpack-cabal =
            { main = "CabalMain.hs"
            , source-dirs = [ "exe/options", "exe/dhall-hpack-cabal" ]
            , dependencies = exe-deps
            }
          , dhall-hpack-json =
            { main = "JsonMain.hs"
            , source-dirs = [ "exe/options", "exe/dhall-hpack-json" ]
            , dependencies = exe-deps
            }
          , dhall-hpack-yaml =
            { main = "YamlMain.hs"
            , source-dirs = [ "exe/options", "exe/dhall-hpack-yaml" ]
            , dependencies = exe-deps
            }
          , dhall-hpack-dhall =
            { main = "DhallMain.hs"
            , source-dirs = [ "exe/options", "exe/dhall-hpack-dhall" ]
            , dependencies = exe-deps
            }
          }
        , tests =
              ./default-tests.dhall
            ⫽ { golden =
                { main = "Golden.hs"
                , source-dirs = [ "test-suite-golden/src" ]
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
                  , "megaparsec >= 9.2.0"
                  , "dhall >= 1.40.2"
                  , "dhall-json >= 1.7.9"
                  , "hpack >= 0.34.6"
                  , "transformers"
                  , "aeson"
                  , "utf8-string"
                  , "directory"
                  ]
                }
              }
        }
