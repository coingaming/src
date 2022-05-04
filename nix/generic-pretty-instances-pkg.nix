{ mkDerivation, base, base16-bytestring, bytestring
, case-insensitive, GenericPretty, hpack, hspec, lib, persistent
, pretty, pretty-simple, proto-lens, proto-lens-runtime
, secp256k1-haskell, text, time, universum, vector
}:
mkDerivation {
  pname = "generic-pretty-instances";
  version = "0.1.0.0";
  src = ./../generic-pretty-instances;
  libraryHaskellDepends = [
    base base16-bytestring bytestring case-insensitive GenericPretty
    persistent pretty pretty-simple proto-lens proto-lens-runtime
    secp256k1-haskell text time universum vector
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base base16-bytestring bytestring case-insensitive GenericPretty
    hspec persistent pretty pretty-simple proto-lens proto-lens-runtime
    secp256k1-haskell text time universum vector
  ];
  prePatch = "hpack";
  homepage = "https://github.com/coingaming/hs/generic-pretty-instances";
  description = "GenericPretty canonical instances";
  license = lib.licenses.bsd3;
}