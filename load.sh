#!/bin/sh

nix-shell -p 'haskellPackages.ghcWithPackages (pkgs: [ (pkgs.callPackage ./stroots.nix {}) ])' --run ghci
