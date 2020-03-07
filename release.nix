let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          aq2ledger = haskellPackagesNew.callPackage ./default.nix {
            # TODO aqbanking is not working in master currently (PR pending)
            # aqbanking = pkgs.aqbanking;
          };
        };
      };
    };
  };

  pkgsOld = import <nixpkgs> { };
  pkgs = import (pkgsOld.fetchFromGitHub {
    owner = "thorstenweber83";
    repo = "nixpkgs";
    rev = "0c462c6b2c773da3590cbee5c3a0359e1200ae5b";
    sha256 = "12miz3rcrw5kkx72yyzsrzrj0b70bylz1nzkbabdkw4c1sb589nn";
  }) { inherit config; };
in {
  aqbanking = pkgs.aqbanking;
  aq2ledger = pkgs.haskellPackages.aq2ledger;
}
