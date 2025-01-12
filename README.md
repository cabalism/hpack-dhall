# `hpack-dhall`

Use hpack's phrasing in dhall to write cabal files.

## Installation

Install with `cabal install hpack-dhall` or [install from
source](https://github.com/cabalism/hpack-dhall/blob/main/docs/BUILDING.md).

## Motivation

There are two main reasons why you'd use hpack-dhall, convenience and safety.

Get the convenience of hpack. Don't bother to state what can be inferred or
defaulted, easing the burden of completing a package description by hand.  For
example `other-modules` can be inferred by taking the set difference between
modules on disk and the set of `exposed-modules`.

Get the safety of dhall's programmable configuration: typed fields, safe imports
and functions.

Another convenience is that files are easily formatted. The cabal file is
formatted on write and the package dhall file can be formatted too with:

    > dhall format --inplace package.dhall

We use hpack phrasing. The vocabulary of [hpack's
fields](https://github.com/cabalism/hpack#top-level-fields) and values differs only
slightly from [cabal's
properties](https://www.haskell.org/cabal/users-guide/developing-packages.html#package-properties).
They're close Haskell packaging dialects.

## Use

This very package is described in
[`package.dhall`](https://github.com/cabalism/hpack-dhall/blob/main/package.dhall)
and produces
[`hpack-dhall.cabal`](https://github.com/cabalism/hpack-dhall/blob/main/package.dhall)
with the command:

```
> dhall-hpack-cabal
generated hpack-dhall.cabal
```

The generated cabal file can then inform both stack and cabal-install what
modules to package into libraries, executables and test suites.

There is [more](https://github.com/cabalism/hpack-dhall/blob/main/docs/MORE.md) you can do.

## Converting from Hpack's `package.yaml`

If you use `stack` to generate a `.cabal` file from a `package.yaml` file, you
can also use `hpack` to do the same. The `dhall-hpack-cabal` executable will go
from `.dhall` to `.cabal` but you can use the `dhall-hpack-yaml` executable to
generate `package.yaml` from a `package.dhall`. That's how we'll manage the
conversion.

If you already have a `package.yaml` file then you can rename this as
`package.dhall`, do the conversion to `.dhall` syntax manually, run `dhall
format package.dhall` to catch syntax errors and then run `dhall-hpack-yaml >
package.yaml` and keep iterating until the generated `package.yaml` is as close
to the original as you need it to be, using something like `git diff` to check
for differences.

## Converting from Cabal's `.cabal`

It will be harder to write a `package.dhall` file from scratch than it is to
convert from a `package.yaml` file but it is doable. All of the field names in
`.dhall` are the same as for `.yaml`. Start small and keep iterating with
`dhall-hpack-cabal` until the generated `.cabal` file is like the original. You
may need to go back to the original to change the formatting and reorder fields
to make comparisons with the generated `.cabal` file easier.

## Source Control

Value the dhall file as a way to produce the cabal file.  If both files are be
checked into source control but the cabal file gets ahead of the dhall file then
it should be possible to get them in sync again.

## Status
![cabal](https://github.com/BlockScope/hpack-dhall/workflows/cabal/badge.svg)
![stack](https://github.com/BlockScope/hpack-dhall/workflows/stack/badge.svg)
[![hackage release](https://img.shields.io/hackage/v/hpack-dhall.svg?label=hackage)](http://hackage.haskell.org/package/hpack-dhall)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/hpack-dhall.svg)](https://hackage.haskell.org/package/hpack-dhall)
