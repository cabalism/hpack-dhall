{ name = "ghc-tcplugins-extra"
, version = "0.5"
, synopsis = "Utilities for writing GHC type-checker plugins"
, description =
    ''
    Utilities for writing GHC type-checker plugins, such as
    creating constraints, with a stable API covering multiple
    GHC releases.''
, category = "Type System"
, author = "Christiaan Baaij"
, maintainer = "christiaan.baaij@gmail.com"
, copyright =
    ''
    Copyright © 2015-2016, University of Twente,
                                2017-2018, QBayLogic''
, github = "clash-lang/ghc-tcplugins-extra"
, license = "BSD2"
, license-file = "LICENSE"
, tested-with =
    "GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5, GHC == 8.8.4, GHC == 8.10.7, GHC == 9.0.2, GHC == 9.2.8, GHC == 9.4.7, GHC == 9.6.6, GHC == 9.8.4, GHC == 9.10.1, GHC == 9.12.1"
, extra-source-files =
  [ "README.md", "CHANGELOG.md", "defaults.dhall", "package.dhall" ]
, ghc-options = [ "-Wall" ]
, flags.deverror
  =
  { description = "Enables `-Werror` for development mode and TravisCI"
  , default = False
  , manual = True
  }
, when =
  [ { condition = "impl(ghc >= 8.0.0)"
    , ghc-options =
      [ "-Wcompat"
      , "-Wincomplete-uni-patterns"
      , "-Widentities"
      , "-Wredundant-constraints"
      ]
    }
  , { condition = "impl(ghc >= 8.4.0)"
    , ghc-options = [ "-fhide-source-paths" ]
    }
  , { condition = "flag(deverror)", ghc-options = [ "-Werror" ] }
  ]
}
