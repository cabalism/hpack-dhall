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

Going from a package, `package.dhall` with [hpack
fields](https://github.com/sol/hpack#top-level-fields) to a
[`*.cabal`](https://www.haskell.org/cabal/users-guide/developing-packages.html#package-descriptions)
package description with
[cabal package
properties](https://www.haskell.org/cabal/users-guide/developing-packages.html#package-properties),
requires less to be stated explicitly, thereby easing the burden of
completing a package description by hand. This is acheived by having sensible
defaults and inferring some fields from others, such as `other-modules` from
`exposed-modules`. By using an hpack-like Dhall dialect here rather
than the [YAML](https://en.wikipedia.org/wiki/YAML) of hpack we're able to;

* Add types to the fields.
* Safely import from other `*.dhall` files.
* Use functions.

So it is a safer and more capable alternative input format for hpack. We're
able to generate the same output but also to generate other outputs. For
instance we can create a `.hlint.yaml` output from a `default-extensions.dhall`
file imported by `package.dhall`.

## Formatting

We can consistently format `package.dhall` using `dhall`;

```
> stack install dhall --stack-yaml=stack-dhall.yaml stack exec dhall -- format
> --inplace package.dhall
```
