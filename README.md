# hpack-dhall

**H**askell **pack**age descriptions in [**Dhall**](https://github.com/dhall-lang/dhall-lang).

A package named `hpack-dhall` containing an executable also named `hpack-dhall`
is described in
[`package.dhall`](https://github.com/sol/hpack-dhall/blob/master/package.dhall).

```
{ name =
    "hpack-dhall"
...
, executable =
    { main = "Main.hs", source-dirs = "driver" }
}
```

When run on `package.dhall`, `hpack-dhall` the executable writes
a `*.cabal` file matching the package name;

```
hpack-dhall> hpack-dhall package.dhall
generated hpack-dhall.cabal
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
to simply describe the package and by leveraging imports and functions we can
do more. For instance we can create an `.hlint.yaml` from
`default-extensions.dhall` shared with and imported by `package.dhall` and
`hlint.dhall`;

```
> cat default-extensions.dhall
{ default-extensions =
    [ "DataKinds"
    , "DeriveFunctor"
    , "DeriveGeneric"
    , "DeriveAnyClass"
    , "DerivingStrategies"
    , "DisambiguateRecordFields"
    , "FlexibleContexts"
    , "FlexibleInstances"
    , "GeneralizedNewtypeDeriving"
    , "GADTs"
    , "LambdaCase"
    , "MultiParamTypeClasses"
    , "MultiWayIf"
    , "NamedFieldPuns"
    , "OverloadedStrings"
    , "PackageImports"
    , "ParallelListComp"
    , "PartialTypeSignatures"
    , "PatternSynonyms"
    , "QuasiQuotes"
    , "RankNTypes"
    , "RecordWildCards"
    , "ScopedTypeVariables"
    , "StandaloneDeriving"
    , "TemplateHaskell"
    , "TypeApplications"
    , "TypeFamilies"
    , "TypeOperators"
    , "TypeSynonymInstances"
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
- -XDeriveGeneric
- -XDeriveAnyClass
- -XDerivingStrategies
- -XDisambiguateRecordFields
- -XFlexibleContexts
- -XFlexibleInstances
- -XGeneralizedNewtypeDeriving
- -XGADTs
- -XLambdaCase
- -XMultiParamTypeClasses
- -XMultiWayIf
- -XNamedFieldPuns
- -XOverloadedStrings
- -XPackageImports
- -XParallelListComp
- -XPartialTypeSignatures
- -XPatternSynonyms
- -XQuasiQuotes
- -XRankNTypes
- -XRecordWildCards
- -XScopedTypeVariables
- -XStandaloneDeriving
- -XTemplateHaskell
- -XTypeApplications
- -XTypeFamilies
- -XTypeOperators
- -XTypeSynonymInstances
- -XTupleSections
- -XUndecidableInstances
```

## Formatting

We can consistently format `package.dhall` and other `*.dhall` imports using
`dhall`;

```
> dhall format --inplace package.dhall
```
