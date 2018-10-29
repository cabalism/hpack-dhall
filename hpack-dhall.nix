{ mkDerivation, aeson, aeson-pretty, base, bytestring, Cabal, dhall
, dhall-json, Diff, filepath, hpack, megaparsec, microlens
, optparse-applicative, prettyprinter, stdenv, tasty, tasty-golden
, text, transformers, utf8-string, yaml
, cabal-install
}:
mkDerivation {
  pname = "hpack-dhall";
  version = "0.4.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring dhall dhall-json filepath hpack
    megaparsec microlens optparse-applicative prettyprinter text
    transformers yaml
  ];
  testHaskellDepends = [
    aeson aeson-pretty base bytestring Cabal dhall dhall-json Diff
    filepath hpack megaparsec microlens prettyprinter tasty
    tasty-golden text transformers utf8-string yaml
  ];
  homepage = "https://github.com/blockscope/hpack-dhall#readme";
  description = "Hpack's dhalling";
  license = stdenv.lib.licenses.bsd3;
  buildTools = [ cabal-install ];
}
