{ pkgs ? (import <nixpkgs> {}).pkgs, compiler ? null }:

with pkgs;
let

  hp = if compiler == null
       then pkgs.haskellPackages
       else pkgs.haskell.packages.${compiler};

in rec {

  xhb-keysyms-build-utils = hp.callPackage ./build-utils {};

  xhb-keysyms-src = callPackage ./xhb-keysyms-src.nix {
    inherit xhb-keysyms-build-utils;
  };

  xhb-keysyms = hp.callPackage ./xhb-keysyms.nix {
    inherit xhb-keysyms-src;
  };

}
