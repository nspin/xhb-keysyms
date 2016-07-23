{ pkgs ? (import <nixpkgs> {}).pkgs, compiler ? null }:

let

  inherit pkgs;

  hp = if compiler == null
       then pkgs.haskellPackages
       else pkgs.haskell.packages.${compiler};

in with pkgs; rec {

  keysymdef-src = fetchurl {
    url = https://cgit.freedesktop.org/xorg/proto/x11proto/plain/keysymdef.h;
    sha256 = "1r3lmn0r3vh4vm5hfgbnr928i4ppqp124bbhfiz2d8yfxhv5mywj";
  };

  xhb-keysyms-build-utils = hp.callPackage ./build-utils {};

  xhb-keysyms-src = stdenv.mkDerivation {

    name = "xhb-keysyms-src";
    version = xhb-keysyms.version;

    copyFiles = [./src ./README.md ./LICENSE ./xhb-keysyms.cabal];

    buildUtils = xhb-keysyms-build-utils;
    ksdSrc = keysymdef-src;

    builder = builtins.toFile "xhb-keysyms-src-builder" ''

      source $stdenv/setup

      mkdir $out

      for f in $copyFiles; do
        cp -r $f $out/$(echo $f | cut -d - -f 2-)
      done

      outdir=$out/gen/Graphics/XHB/KeySym
      mkdir -p $outdir/Names

      cat $ksdSrc | $buildUtils/bin/gen-xhb-keysyms $outdir/Defs.hs $outdir/Names/Internal.hs

      # validation
      grep '::' $outdir/Defs.hs | cut -d ' ' -f 1 | cut -c 2- > foo
      thediff=$(cat $ksdSrc | grep '^#define XK_' | cut -d ' ' -f 2 | cut -c 2- | diff foo -)
      [ -z "$thediff" ]

    '';

  };

  xhb-keysyms =
    let f = { mkDerivation, base, stdenv, xhb }:
              mkDerivation {
                pname = "xhb-keysyms";
                version = "0.1";
                src = xhb-keysyms-src;
                libraryHaskellDepends = [ base xhb ];
                license = stdenv.lib.licenses.mit;
              };
    in hp.callPackage f {};

}
