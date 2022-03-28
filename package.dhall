let deps =
      [ "aeson"
      , "aeson-pretty"
      , "base >= 4.13 && < 5"
      , "bytestring"
      , "dhall >= 1.41.1"
      , "dhall-json >= 1.7.10"
      , "filepath"
      , "hpack ^>= 0.34.7"
      , "megaparsec >= 9.2"
      , "microlens"
      , "prettyprinter"
      , "text"
      , "transformers"
      , "yaml"
      ]

in  let exe-deps = [ "optparse-applicative" ]

    in  { name = "hpack-dhall"
        , version = "0.5.5"
        , maintainer = "Phil de Joux <phil.dejoux@blockscope.com>"
        , copyright =
            "© 2018 - 2022 Phil de Joux, © 2018 - 2022 Block Scope Limited"
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
        , tested-with = "GHC == 8.8.4, GHC == 8.10.7, GHC == 9.0.1"
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
                  [ "Cabal"
                  , "Diff"
                  , "directory"
                  , "tasty"
                  , "tasty-golden"
                  , "utf8-string"
                  ]
                }
              }
        }
