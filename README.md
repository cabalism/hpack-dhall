# `hpack-dhall`

Use hpack phrasing in dhall to write cabal files.

## Installation

Install with `cabal install hpack-dhall` or [install from
source](https://github.com/cabalism/hpack-dhall/blob/main/BUILDING.md).

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

    $ dhall format --inplace package.dhall

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
$ dhall-hpack-cabal
generated hpack-dhall.cabal
```

The generated cabal file can then inform both stack and cabal-install what
modules to package into libraries, executables and test suites.

There is [more](https://github.com/cabalism/hpack-dhall/blob/main/MORE.md) you can do.

## Source Control

Value the dhall file as a way to produce the cabal file.  If both files are be
checked into source control but the cabal file gets ahead of the dhall file then
it should be possible to get them in sync again.

## Status
![cabal](https://github.com/BlockScope/hpack-dhall/workflows/cabal/badge.svg)
![stack](https://github.com/BlockScope/hpack-dhall/workflows/stack/badge.svg)
[![hackage release](https://img.shields.io/hackage/v/hpack-dhall.svg?label=hackage)](http://hackage.haskell.org/package/hpack-dhall)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/hpack-dhall.svg)](https://hackage.haskell.org/package/hpack-dhall)