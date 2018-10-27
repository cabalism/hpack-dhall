{ mkDerivation, aeson, base, bytestring, Cabal, dhall, dhall-json
, Diff, filepath, hpack, megaparsec, microlens, prettyprinter
, stdenv, tasty, tasty-golden, text, transformers
, cabal-install
}:
mkDerivation {
  pname = "hpack-dhall";
  version = "0.4.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base dhall dhall-json filepath hpack megaparsec microlens
    text transformers
  ];
  testHaskellDepends = [
    aeson base bytestring Cabal dhall dhall-json Diff filepath hpack
    megaparsec microlens prettyprinter tasty tasty-golden text
    transformers
  ];
  homepage = "https://github.com/blockscope/hpack-dhall#readme";
  description = "Dhall support for Hpack";
  license = stdenv.lib.licenses.bsd3;
  buildTools = [ cabal-install ];
}
