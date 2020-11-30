{ pkgs ? import <nixpkgs> {}, hpkgs ? pkgs.haskellPackages }:
hpkgs.shellFor {
  packages = p: [ (import ./default.nix {}) ];
  withHoogle = true;
}
