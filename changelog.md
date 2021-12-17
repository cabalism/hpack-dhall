The [latest version](https://github.com/blockscope/hpack-dhall/blob/master/changelog.md) of this changelog.

## 0.5.4 - Rewrite the README
* Require `hpack >= 0.34.6`.
* Test with GHC `8.8.4`, GHC `8.10.7` and GHC `9.0.1`.

## 0.5.3 - Rewrite the README
* Rewrite the README, making it shorter and splitting some details about more
  uses and building into separate docs.
* Require `hpack >= 0.34.4`:
  ```diff
  $ dhall-hpack-cabal --version
  -dhall-hpack-cabal-0.5.2
  -hpack-0.34.2
  +dhall-hpack-cabal-0.5.3
  +hpack-0.34.4
  ```
* Require `base >= 4.13`, implying `GHC >= 8.8.4`.
* Test with GHC `8.8.4` and `8.10.4`.
* Add files for different stack GHC versions.
  ```
  $ stack build --stack-yaml=stack/stack-8.8.4.yaml
  $ stack build --stack-yaml=stack/stack-8.10.4.yaml
  $ stack build --stack-yaml=stack/stack-9.0.1.yaml
  ```
* Remove the travis script and update github scripts with:
  ```diff
  - - uses: actions/setup-haskell@v1
  + - uses: haskell/actions/setup@v1

  - - uses: actions/cache@v1
  + - uses: actions/cache@v2
  ```
* Remove stale nix-related files.

## 0.5.2 - Consistent Golden Tests
* Use explicit dependencies to achieve consistent golden tests in all but
  stack-8.6.3.yaml.

## 0.5.1 - Minor, bump in hpack version 
* Regenerate golden files for the bump in hpack's version:

```
---- This file has been generated from package.yaml by hpack version 0.31.0.
++-- This file has been generated from package.yaml by hpack version 0.31.1.
```

## 0.5.0 - Sorted Fields Pretty Printing
* Sort fields when pretty printing JSON and YAML.
* Add real world golden tests, using stack and hpack packages.

## 0.4.0 - Split Executables
* Add licence and copyright.
* Rename hpack-dhall to dhall-hpack-cabal.
* Add dhall-hpack-* executables for showing dhall, json and yaml.
