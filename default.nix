{ pkgs ? import <nixpkgs> {}
, hpkgs ? pkgs.haskellPackages
, mkDerivation ? hpkgs.mkDerivation
}: hpkgs.callCabal2nix "loquate" (pkgs.lib.cleanSource ./.) { inherit mkDerivation; }
