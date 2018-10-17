# hpack-dhall

**H**askell **pack**age descriptions in [**Dhall**](https://github.com/dhall-lang/dhall-lang).

A package named `hpack-dhall` containing an executable also named `hpack-dhall` is described in
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

By going from [hpack package fields](https://github.com/sol/hpack#top-level-fields) to
[cabal package properties](https://www.haskell.org/cabal/users-guide/developing-packages.html#package-properties),
we are not required to state what can be inferred or defaulted,
easing the burden of completing a package description by hand.
For example `other-modules` can be inferred by taking the set difference between
modules on disk and the set of `exposed-modules`.

By using an hpack-like Dhall dialect here rather
than the [YAML](https://en.wikipedia.org/wiki/YAML) of hpack we're able to;

* Add types to the fields.
* Safely import from other `*.dhall` files.
* Use functions.

With this safer and more capable alternative input format for hpack, we're
able to simply describe the package and by leveraging imports and functions we're
also able to do more. For
instance we can create an `.hlint.yaml` from `default-extensions.dhall`
shared with and imported by `package.dhall`.

## Formatting

We can consistently format `package.dhall` using `dhall`;

```
> stack install dhall --stack-yaml=stack-dhall.yaml stack exec dhall -- format
> --inplace package.dhall
```
