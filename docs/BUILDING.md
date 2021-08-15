# Building from Source

```
$ gh repo clone cabalism/hpack-dhall
$ cd hpack-dhall

# installing with stack (stack itself bundles hpack)
$ stack --version
Version 2.7.3, Git revision 7927a3aec32e2b2e5e4fb5be76d0d50eddcc197f x86_64 hpack-0.34.4
$ stack install
Copied executables to /Users/pdejoux/.local/bin:
- dhall-hpack-cabal
- dhall-hpack-dhall
- dhall-hpack-json
- dhall-hpack-yaml 

# installing with cabal
$ ghcup --version
The GHCup Haskell installer, version v0.1.16.2
$ ghcup set cabal 3.4.0.0
[ Info  ] Cabal 3.4.0.0 successfully set as default version
$ ghcup set ghc 8.10.4
[ Info  ] GHC 8.10.4 successfully set as default version
$ cabal install all:exes --overwrite-policy=always --installdir=$HOME/.cabal/bin
...
Completed    hpack-dhall-0.5.3 (exe:dhall-hpack-cabal)
Symlinking 'dhall-hpack-dhall' to '/Users/.../.cabal/bin/dhall-hpack-dhall'
Symlinking 'dhall-hpack-json' to '/Users/.../.cabal/bin/dhall-hpack-json'
Symlinking 'dhall-hpack-yaml' to '/Users/.../.cabal/bin/dhall-hpack-yaml'
Symlinking 'dhall-hpack-cabal' to '/Users/.../.cabal/bin/dhall-hpack-cabal'
```