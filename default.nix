{ mkDerivation, aeson, ansi-terminal, base, bytestring, Decimal, directory
, filepath, hledger-lib, mtl, optparse-applicative, parsec, process, protolude
, QuickCheck, quickcheck-instances, stdenv, text, yaml }:
# TODO aqbanking is not working in master currently (PR pending)
# , aqbanking }:
mkDerivation {
  pname = "aq2ledger";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    ansi-terminal
    base
    bytestring
    Decimal
    directory
    filepath
    hledger-lib
    mtl
    optparse-applicative
    parsec
    process
    protolude
    QuickCheck
    quickcheck-instances
    text
    yaml
  ];
  executableHaskellDepends = [
    aeson
    ansi-terminal
    base
    bytestring
    Decimal
    directory
    filepath
    hledger-lib
    mtl
    optparse-applicative
    parsec
    process
    protolude
    QuickCheck
    quickcheck-instances
    text
    yaml
  ];
  # TODO aqbanking is not working in master currently (PR pending)
  # executableSystemDepends = [ aqbanking ];
  doHaddock = false;
  homepage = "https://github.com/dpaetzel/aq2ledger";
  description = "Automates plain text accounting data entry in ledger format";
  license = stdenv.lib.licenses.gpl3;
}
