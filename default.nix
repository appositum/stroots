let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./stroots.nix {}
