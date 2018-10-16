{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, dhall, dhall-json, hpack, hspec
      , interpolate, megaparsec, mockery, stdenv, text, transformers
      , cabal-install
      }:
      mkDerivation {
        pname = "hpack-dhall";
        version = "0.3.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base dhall dhall-json hpack megaparsec text transformers
        ];
        testHaskellDepends = [
          aeson base dhall dhall-json hpack hspec interpolate megaparsec
          mockery text transformers
        ];
        homepage = "https://github.com/sol/hpack-dhall#readme";
        description = "Dhall support for Hpack";
        license = stdenv.lib.licenses.publicDomain;
        buildTools = [ cabal-install ];
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
