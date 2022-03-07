-- SEE: https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Driver/Session.hs#L1376-1424
{ name = "with-GHC2021"
, default-extensions =
    https://raw.githubusercontent.com/cabalism/hpack-dhall/main/dhall/ghc-2021.dhall
, executable = { main = "Main.hs", source-dirs = "src" }
}
