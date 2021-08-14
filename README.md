# Hpack Dhall

Use hpack phrasing in dhall to write cabal files.

## Installation

Install with `cabal install hpack-dhall` or [install from
source](https://github.com/cabalism/hpack-dhall/blob/main/BUILDING.md).

## Convenience and Safety

There are two main reasons why you'd use hpack-dhall, convenience and safety.

Get the convenience of hpack. Don't bother to state what can be inferred or
defaulted, easing the burden of completing a package description by hand.  For
example `other-modules` can be inferred by taking the set difference between
modules on disk and the set of `exposed-modules`.

Get the safety of dhall's programmable configuration: typed fields, safe imports
and functions.

Another convenience is that files are easily formatted. The cabal file is
formatted on write and the package dhall file can be formatted too with:

    $ dhall format --inplace package.dhall

## Phrasing

Use hpack phrasing: The vocabulary of [hpack's
fields](https://github.com/cabalism/hpack#top-level-fields) and values differs only
slightly from [cabal's
properties](https://www.haskell.org/cabal/users-guide/developing-packages.html#package-properties).
They're close Haskell packaging dialects.

## Write Cabal

This very package is described in
[`package.dhall`](https://github.com/cabalism/hpack-dhall/blob/main/package.dhall)
and produces
[`hpack-dhall.cabal`](https://github.com/cabalism/hpack-dhall/blob/main/package.dhall)
with the command:

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
