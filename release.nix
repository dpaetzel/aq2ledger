let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          buchhaltung2 = haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
in { buchhaltung2 = pkgs.haskellPackages.buchhaltung2; }
