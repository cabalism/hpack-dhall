# hpack-dhall: **H**askell **pack**age description in **dhall**

In
[`package.dhall`](https://github.com/sol/hpack-dhall/blob/master/package.dhall)
is described a package named `hpack-dhall` also with an executable of the same
name.  That executable when built and run on the `package.dhall` creates
`hpack-dhall.cabal`, a `*.cabal` file with a name matching the package name. It
translates from a package described in `*.dhall` to one described
[`*.cabal`](https://www.haskell.org/cabal/users-guide/developing-packages.html#package-descriptions)
package descriptions. The fields of the description are [hpack
fields](https://github.com/sol/hpack#top-level-fields). These differ from the
[cabal package
properties](https://www.haskell.org/cabal/users-guide/developing-packages.html#package-properties),
requiring less to be stated explicitly and thereby easing the burden of
completing a package description by hand. This is acheived by having sensible
defaults and inferring some fields from others, such as `other-modules` from
`exposed-modules`. By using an hpack-like [Dhall](https) dialect here rather
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
