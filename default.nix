{ pkgs ? import <nixpkgs> {}, hpkgs ? pkgs.haskellPackages, mkDerivation ? hpkgs.mkDerivation }:
hpkgs.callCabal2nix "loquacious" ./. { inherit mkDerivation; }
