The [latest version](https://github.com/blockscope/hpack-dhall/blob/master/changelog.md) of this changelog.

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
