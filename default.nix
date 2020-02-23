{ mkDerivation, aeson, base, bytestring, Decimal, directory, filepath
, hledger-lib, mtl, parsec, process, protolude, QuickCheck, quickcheck-instances
, stdenv, text }:
# TODO aqbanking is not working in master currently (PR pending)
# , aqbanking }:
mkDerivation {
  pname = "buchhaltung2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    Decimal
    directory
    filepath
    hledger-lib
    mtl
    parsec
    process
    protolude
    QuickCheck
    quickcheck-instances
    text
  ];
  executableHaskellDepends = [
    aeson
    base
    bytestring
    Decimal
    directory
    filepath
    hledger-lib
    mtl
    parsec
    process
    protolude
    QuickCheck
    quickcheck-instances
    text
  ];
  # TODO aqbanking is not working in master currently (PR pending)
  # executableSystemDepends = [ aqbanking ];
  doHaddock = false;
  homepage = "https://github.com/dpaetzel/buchhaltung2";
  description = "Automates plain text accounting data entry in ledger format";
  license = stdenv.lib.licenses.gpl3;
}
