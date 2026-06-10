let deps =
      [ "aeson <3"
      , "aeson-pretty <1"
      , "base >=4.13 && <5"
      , "bytestring <0.13"
      , "dhall >=1.41.1 && <2"
      , "dhall-json >=1.7.10 && <2"
      , "filepath <2"
      , "hpack >=0.39.6 && <1"
      , "megaparsec >=9.2 && <10"
      , "microlens <0.6"
      , "prettyprinter <2"
      , "text <3"
      , "transformers <0.7"
      , "yaml <0.12"
      ]

let exe-deps = [ "optparse-applicative <0.20" ]

let hook-deps = [ "extra <2", "filepattern <1", "optparse-applicative <0.20" ]

in  { name = "hpack-dhall"
    , version = "0.6.0"
    , maintainer = "Phil de Joux <phil.dejoux@blockscope.com>"
    , copyright =
        "© 2018 - 2026 Phil de Joux, © 2018 - 2026 Block Scope Limited"
    , license = "BSD3"
    , license-file = "LICENSE"
    , category = "Development"
    , synopsis = "Hpack with Dhall"
    , description =
        ''
        Hpack-Dhall brings functions, types and file imports to Haskell
        packaging.  It is Hpack with Dhall.
        ''
    , github = "cabalism/hpack-dhall"
    , tested-with =
        "GHC == 9.2.8, GHC == 9.4.8, GHC == 9.6.7, GHC == 9.8.4, GHC == 9.10.3, GHC == 9.12.4"
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
      { cabal-dpack =
        { main = "DpackMain.hs"
        , source-dirs = [ "exe", "hook", "hook/cabal-dpack" ]
        , dependencies = hook-deps
        }
      , cabal-diy-pack =
        { main = "DiyPackMain.hs"
        , source-dirs = [ "exe", "hook", "hook/cabal-diy-pack" ]
        , dependencies = hook-deps
        }
      , cabal-ypack =
        { main = "YpackMain.hs"
        , source-dirs = [ "exe", "hook", "hook/cabal-ypack" ]
        , dependencies = hook-deps
        }
      , dhall-hpack-cabal =
        { main = "CabalMain.hs"
        , source-dirs = [ "exe", "exe/dhall-hpack-cabal" ]
        , dependencies = exe-deps
        }
      , dhall-hpack-json =
        { main = "JsonMain.hs"
        , source-dirs = [ "exe", "exe/dhall-hpack-json" ]
        , dependencies = exe-deps
        }
      , dhall-hpack-yaml =
        { main = "YamlMain.hs"
        , source-dirs = [ "exe", "exe/dhall-hpack-yaml" ]
        , dependencies = exe-deps
        }
      , dhall-hpack-dhall =
        { main = "DhallMain.hs"
        , source-dirs = [ "exe", "exe/dhall-hpack-dhall" ]
        , dependencies = exe-deps
        }
      , yaml-hpack-cabal =
        { main = "CabalMain.hs"
        , source-dirs = [ "exe", "exe/yaml-hpack-cabal" ]
        , dependencies = exe-deps
        }
      }
    , tests.golden
      =
      { main = "Golden.hs"
      , source-dirs = [ "test-suite-golden/src" ]
      , dependencies =
        [ "Cabal", "Diff", "directory", "tasty", "tasty-golden", "utf8-string" ]
      }
    }
