import <nixpkgs> {
  config = { pkgs }: {
    haskellPackageOverrides = self: super: with pkgs.haskell.lib; {
      xhb = appendPatch super.xhb ./xhb.patch;
      xhb-keysyms = self.callPackage ./xhb-keysyms.nix {};
      xhb-keysyms-src = self.callPackage ./xhb-keysyms-src.nix {};
      xhb-keysyms-build-utils = self.callPackage ./build-utils {};
    };
  };
}
