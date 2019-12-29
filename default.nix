{ mkDerivation, aeson, base, bytestring, directory, filepath, hledger-lib, mtl
, process, protolude, QuickCheck, stdenv, text, aqbanking }:
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
    directory
    filepath
    hledger-lib
    mtl
    process
    protolude
    QuickCheck
    text
  ];
  executableHaskellDepends = [
    aeson
    base
    bytestring
    directory
    filepath
    hledger-lib
    mtl
    process
    protolude
    QuickCheck
    text
  ];
  executableSystemDepends = [ aqbanking ];
  doHaddock = false;
  homepage = "https://github.com/dpaetzel/buchhaltung2";
  description = "Automates plain text accounting data entry in ledger format";
  license = stdenv.lib.licenses.gpl3;
}
