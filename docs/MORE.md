
# More Uses

Aside from `dhall-hpack-cabal`, three other executables are bundled with this
package. If you're migrating from hpack to hpack-dhall but don't have a source
controlled cabal file then `dhall-hpack-yaml` can be used to generate a
package.yaml.  If for some reason you want JSON, then `dhall-hpack-json` does
that.  For seeing dhall with imports resolved there's `dhall-hpack-dhall`.

## Configure Linting Extensions

By using imports and functions we can do more than just create a cabal file. We
can configure extensions for linting:

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

We can pull those same `default-extensions` into a package description:

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

## Conditional Restrictions

Using hpack's [conditionals](https://github.com/sol/hpack#conditionals) in
a list in `package.dhall` can cause an error because lists in dhall must have
elements of the same type:

```
# package.yaml
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

This can be represented as:
```
# package.dhall
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
