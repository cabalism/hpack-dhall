CABAL_VIA ?= dhall2cabal

# How to generate project-nix/ghc-$(GHC_VERSION)/sha256map.nix?
# This is copied from ghc-$(GHC_VERSION).sha256map.nix.
#  - false to generate from *.dhall inputs via sha256map.hs.
#  - true to generate from stack.yaml via sha256map.py.
SHA256MAP_VIA_PYTHON ?= false

# To use installed executables instead of *.hs scripts, set these to true.
SHA256MAP_HS_EXE ?= false
PKG_GROUPS_HS_EXE ?= false
PKGS_SORTED_HS_EXE ?= false
PKGS_UPGRADE_DONE_HS_EXE ?= false

include project-versions.mk
include updo/Makefile

project-nix/ghc-%/sha256map.nix: ghc-%.sha256map.nix
	mkdir -p $(@D) && cp $^ $@

.PHONY: all
all: \
  projects \
  project-nix/ghc-$(GHC_VERSION)/sha256map.nix \
  project-versions.nix

# To make stack.yaml or cabal.project and no other, mark the file we copy from
# as intermediate. This is all we want when not doing a GHC upgrade.
#
# Comment out these .INTERMEDIATE targets to allow these files to be kept.
.INTERMEDIATE: ghc-$(GHC_VERSION).$(CABAL_VIA).project
.INTERMEDIATE: ghc-$(GHC_UPGRADE).$(CABAL_VIA).project
.INTERMEDIATE: ghc-$(GHC_VERSION).$(STACK_VIA).yaml
.INTERMEDIATE: ghc-$(GHC_UPGRADE).$(STACK_VIA).yaml
.INTERMEDIATE: ghc-$(GHC_VERSION).sha256map.nix
.INTERMEDIATE: ghc-$(GHC_UPGRADE).sha256map.nix

# If true, generate the sha256map from the stack.yaml with python,
# overriding the recipe for this target.
ifeq ($(SHA256MAP_VIA_PYTHON), true)
ghc-$(GHC_VERSION).sha256map.nix: stack.yaml
	updo/project-nix/sha256map.py <$^ >$@
ghc-$(GHC_UPGRADE).sha256map.nix: stack.upgrade.yaml
	updo/project-nix/sha256map.py <$^ >$@
endif

.DEFAULT_GOAL := all

UPDO_VERSION ?= a86c165687b2b5e8251265418a9c4181c6a651a0
UPDO_URL := https://github.com/cabalism/updo/archive/${UPDO_VERSION}.tar.gz

updo/Makefile:
	rm -rf updo
	curl -sSL ${UPDO_URL} | tar -xz
	mv updo-* updo
	chmod +x $$(grep -RIl '^#!' updo)