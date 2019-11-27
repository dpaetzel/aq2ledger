{ mkDerivation, base, protolude, stdenv }:
mkDerivation {
  pname = "buchhaltung2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base protolude ];
  homepage = "https://github.com/dpaetzel/buchhaltung2";
  description = "Automates plain text accounting data entry in ledger format";
  license = stdenv.lib.licenses.gpl3;
}
