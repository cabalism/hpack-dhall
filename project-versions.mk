# Versions of GHC and stackage resolver, the ones we're on and the next ones
# we're upgrading to.
# GHC_VERSION ?= 9.2.8
# STACKAGE_VERSION ?= lts-20.26
# GHC_VERSION ?= 9.4.8
# STACKAGE_VERSION ?= lts-21.25
# GHC_VERSION ?= 9.6.7
# STACKAGE_VERSION ?= lts-22.44
# GHC_VERSION ?= 9.8.4
# STACKAGE_VERSION ?= lts-23.28
# GHC_VERSION ?= 9.10.3
# STACKAGE_VERSION ?= lts-24.36
GHC_VERSION ?= 9.12.4
STACKAGE_VERSION ?= nightly-2026-04-12

# For the upgrade, pick a matching pair of ghc-version and stack resolver.

# Imports can be relative to the project or relative to importing file.
# ImportRelative works with cabal-3.10 and is the default.
# ProjectRelative works with cabal-3.8.
CABAL_RELATIVITY ?= ImportRelative