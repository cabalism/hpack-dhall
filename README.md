# hpack-dhall

![cabal](https://github.com/BlockScope/hpack-dhall/workflows/cabal/badge.svg)
![stack](https://github.com/BlockScope/hpack-dhall/workflows/stack/badge.svg)
[![hackage release](https://img.shields.io/hackage/v/hpack-dhall.svg?label=hackage)](http://hackage.haskell.org/package/hpack-dhall)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/hpack-dhall.svg)](https://hackage.haskell.org/package/hpack-dhall)

[**Dhall**](https://github.com/dhall-lang/dhall-lang) programmable configuration for packaging Haskell modules in the [hpack](https://github.com/sol/hpack) dialect.

This package named `hpack-dhall` as described in
[`package.dhall`](https://github.com/sol/hpack-dhall/blob/master/package.dhall).

```
{ name =
    "hpack-dhall"
...
, library =
    { exposed-modules = "Hpack.Dhall" }
, executables =
    { dhall-hpack-cabal = ...
    , dhall-hpack-json = ...
    , dhall-hpack-yaml = ...
    , dhall-hpack-dhall = ...
    }
}
```

This `.cabal` creating executable can be run over its own package description;

```
> stack install --stack-yaml=stack-8.4.4.yaml
...
Copied executables to /.../hpack-dhall/__bin:
- dhall-hpack-cabal
- dhall-hpack-dhall
- dhall-hpack-json
- dhall-hpack-yaml

> __bin/dhall-hpack-cabal package.dhall
hpack-dhall.cabal is up-to-date

> __bin/dhall-hpack-cabal --force package.dhall
generated hpack-dhall.cabal
```

Using one of the golden tests for example, there are executables to show the
dhall with the imports made as well as json and yaml equivalents;
```
> __bin/dhall-hpack-dhall test/golden/hpack-dhall-cabal/empty-package.dhall
{ name = "empty-package" }

> __bin/dhall-hpack-json test/golden/hpack-dhall-cabal/empty-package.dhall
{
    "name": "empty-package"
}

> __bin/dhall-hpack-yaml test/golden/hpack-dhall-cabal/empty-package.dhall
name: empty-package
```

By going from [hpack package
fields](https://github.com/sol/hpack#top-level-fields) to [cabal package
properties](https://www.haskell.org/cabal/users-guide/developing-packages.html#package-properties),
we are not required to state what can be inferred or defaulted, easing the
burden of completing a package description by hand.  For example
`other-modules` can be inferred by taking the set difference between modules on
disk and the set of `exposed-modules`.

By using an hpack-like Dhall dialect here rather than the
[YAML](https://en.wikipedia.org/wiki/YAML) of hpack we're able to;

* Add types to the fields.
* Safely import from other `*.dhall` files.
* Use functions.

## Imports and Functions

With this safer and more capable alternative input format for hpack, we're able
to simply describe the package and by using imports and functions we can do
more such as configuring linting;

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

## Formatting

We can consistently format `package.dhall` and other `*.dhall` imports using
`dhall`;

```
> stack install dhall --stack-yaml=stack-dhall.yaml
> __bin/dhall format --inplace package.dhall
```

## Restrictions

Using hpack's [conditionals](https://github.com/sol/hpack#conditionals) in
a list in `package.dhall` can cause an error because lists in Dhall must have
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
