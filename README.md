# Hpack Dhall

Use hpack phrasing in dhall to write cabal files.

## Convenience and Safety

There are two main reasons why you'd use hpack-dhall, convenience and safety.

Get the convenience of hpack. Don't bother to state what can be inferred or
defaulted, easing the burden of completing a package description by hand.  For
example `other-modules` can be inferred by taking the set difference between
modules on disk and the set of `exposed-modules`.

Get the safety of dhall's programmable configuration: typed fields, safe imports
and functions.

Another convenience is that files are easily formatted. The cabal output file is
formatted on write and the input package dhall file can be formatted too with:

    $ dhall format --inplace package.dhall

## Phrasing

Use hpack phrasing: The vocabulary of [hpack's
fields](https://github.com/sol/hpack#top-level-fields) and values differs only
slightly from [cabal's
properties](https://www.haskell.org/cabal/users-guide/developing-packages.html#package-properties).
They're close Haskell packaging dialects.

## Write Cabal

This very package is described in
[`package.dhall`](https://github.com/sol/hpack-dhall/blob/master/package.dhall).

```
{ name =
    "hpack-dhall"
...
, library =
    { exposed-modules = "Hpack.Dhall" }
, executables =
    { dhall-hpack-cabal = ...
    }
}
```

From the same folder the cabal file can be produced with:

```
$ dhall-hpack-cabal
generated hpack-dhall.cabal
```

This cabal file can be consumed by stack and cabal-install to build the package.

## Source Control

Value the dhall file as a way to produce the cabal file. It's a convenience.
Both files should be checked into source control. If you're working on a project
where some people like the dhall file and others like the cabal file then work
can continue without disrupting either party. If the cabal file gets ahead of
the dhall file, it should be possible to get them in sync again. By checking
both types of file into source control you're not excluding either way of
working.

## Building from Source

```
$ gh repo clone cabalism/hpack-dhall
$ cd hpack-dhall

# installing with stack (stack itself bundles hpack)
$ stack --version
Version 2.7.3, Git revision 7927a3aec32e2b2e5e4fb5be76d0d50eddcc197f x86_64 hpack-0.34.4
$ stack install
Copied executables to /Users/pdejoux/.local/bin:
- dhall-hpack-cabal
- dhall-hpack-dhall
- dhall-hpack-json
- dhall-hpack-yaml 

# installing with cabal
$ ghcup --version
The GHCup Haskell installer, version v0.1.16.2
$ ghcup set cabal 3.4.0.0
[ Info  ] Cabal 3.4.0.0 successfully set as default version
$ ghcup set ghc 8.10.4
[ Info  ] GHC 8.10.4 successfully set as default version
$ cabal install all:exes --overwrite-policy=always --installdir=$HOME/.cabal/bin
...
Completed    hpack-dhall-0.5.3 (exe:dhall-hpack-cabal)
Symlinking 'dhall-hpack-dhall' to '/Users/.../.cabal/bin/dhall-hpack-dhall'
Symlinking 'dhall-hpack-json' to '/Users/.../.cabal/bin/dhall-hpack-json'
Symlinking 'dhall-hpack-yaml' to '/Users/.../.cabal/bin/dhall-hpack-yaml'
Symlinking 'dhall-hpack-cabal' to '/Users/.../.cabal/bin/dhall-hpack-cabal'
```

## Other Exes

If you're migrating from hpack to hpack-dhall but don't have a source controlled cabal file then `dhall-hpack-yaml` can be used to generate a package.yaml.

If for some reason you want JSON, then `dhall-hpack-json` does that.

For seeing dhall with imports resolved there's `dhall-hpack-dhall`.

## Imports and Functions

By using imports and functions we can do more than just create a cabal file. We
can configure extensions for linting;

```
> cat default-extensions.dhall
{ default-extensions =
    [ "DataKinds"
    , "DeriveFunctor"
    ...
    , "TupleSections"
    , "UndecidableInstances"
    ]
}

> cat hlint.dhall
    let Prelude/List/map =
          https://raw.githubusercontent.com/dhall-lang/Prelude/35deff0d41f2bf86c42089c6ca16665537f54d75/List/map

in  let defs = ./default-extensions.dhall

in  let f = λ(s : Text) → "-X" ++ s

in  { arguments = Prelude/List/map Text Text f defs.default-extensions }

> dhall-to-yaml < ./hlint.dhall > ./.hlint.yaml

> cat .hlint.yaml
arguments:
- -XDataKinds
- -XDeriveFunctor
...
- -XTupleSections
- -XUndecidableInstances
```

We can pull those same `default-extensions` into a package description;

```
> cat package.dhall
    let defs = ./defaults.dhall

in    defs
    ⫽ ./default-extensions.dhall
    ⫽ { name =
          "flight-units"
    ...
      , github =
          "blockscope/flare-timing/units"
    ...
      , dependencies =
            defs.dependencies
          # [ "numbers"
            , "fixed"
            , "bifunctors"
            , "text"
            , "formatting"
            , "uom-plugin"
            , "siggy-chardust"
            ]
    ...
      }
```

## Restrictions

Using hpack's [conditionals](https://github.com/sol/hpack#conditionals) in
a list in `package.dhall` can cause an error because lists in dhall must have
elements of the same type;

From stack's `package.yaml`;
```
executables:
  stack:
    main: Main.hs
    source-dirs: src/main
    generated-other-modules:
    - Build_stack
    - Paths_stack
    ghc-options:
    - -threaded
    - -rtsopts
    dependencies:
    - stack
    when:
    - condition: flag(static)
      ld-options:
      - -static
      - -pthread
    - condition: ! '!(flag(disable-git-info))'
      cpp-options: -DUSE_GIT_INFO
      dependencies:
      - githash
      - optparse-simple
    - condition: flag(hide-dependency-versions)
      cpp-options: -DHIDE_DEP_VERSIONS
    - condition: flag(supported-build)
      cpp-options: -DSUPPORTED_BUILD
```

This can be represented in `package.dhall` as;
```
, executables =
    { stack =
        { main =
            "Main.hs"
        , source-dirs =
            [ "src/main" ]
        , generated-other-modules =
            [ "Build_stack", "Paths_stack" ]
        , ghc-options =
            [ "-threaded", "-rtsopts" ]
        , dependencies =
            [ "stack" ]
        , when =
            [ { condition =
                  "flag(static)"
              , cpp-options =
                  [] : List Text
              , dependencies =
                  [] : List Text
              , ld-options =
                  [ "-static", "-pthread" ]
              }
            , { condition =
                  "!(flag(disable-git-info))"
              , cpp-options =
                  [ "-DUSE_GIT_INFO" ]
              , dependencies =
                  [ "githash", "optparse-simple" ]
              , ld-options =
                  [] : List Text
              }
            , { condition =
                  "flag(hide-dependency-versions)"
              , cpp-options =
                  [ "-DHIDE_DEP_VERSIONS" ]
              , dependencies =
                  [] : List Text
              , ld-options =
                  [] : List Text
              }
            , { condition =
                  "flag(supported-build)"
              , cpp-options =
                  [ "-DSUPPORTED_BUILD" ]
              , dependencies =
                  [] : List Text
              , ld-options =
                  [] : List Text
              }
            ]
        }
    }
```

## Status
![cabal](https://github.com/BlockScope/hpack-dhall/workflows/cabal/badge.svg)
![stack](https://github.com/BlockScope/hpack-dhall/workflows/stack/badge.svg)
[![hackage release](https://img.shields.io/hackage/v/hpack-dhall.svg?label=hackage)](http://hackage.haskell.org/package/hpack-dhall)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/hpack-dhall.svg)](https://hackage.haskell.org/package/hpack-dhall)
