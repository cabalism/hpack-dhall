{ mkDerivation, aeson, base, dhall, dhall-json, hpack, hspec
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
}
