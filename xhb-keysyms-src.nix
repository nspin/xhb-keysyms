{ stdenv, fetchurl, xhb-keysyms-build-utils }:

stdenv.mkDerivation {

  name = "xhb-keysyms-src";
  version = xhb-keysyms-build-utils.version;

  copyFiles = [./src ./README.md ./LICENSE ./xhb-keysyms.cabal];

  buildUtils = xhb-keysyms-build-utils;

  ksdSrc = fetchurl {
    url = https://cgit.freedesktop.org/xorg/proto/x11proto/plain/keysymdef.h;
    sha256 = "1r3lmn0r3vh4vm5hfgbnr928i4ppqp124bbhfiz2d8yfxhv5mywj";
  };

  builder = builtins.toFile "xhb-keysyms-src-builder" ''

    source $stdenv/setup

    mkdir $out

    for f in $copyFiles; do
      cp -r $f $out/$(echo $f | cut -d - -f 2-)
    done

    outdir=$out/gen/Graphics/XHB/KeySym
    mkdir -p $outdir/Names
    mkdir -p $outdir/Alph

    cat $ksdSrc | $buildUtils/bin/gen-xhb-keysyms $outdir/Defs.hs $outdir/Names/Internal.hs $outdir/Alph/Internal.hs

    # validation
    grep '::' $outdir/Defs.hs | cut -d ' ' -f 1 | cut -c 2- > foo
    thediff=$(cat $ksdSrc | grep '^#define XK_' | cut -d ' ' -f 2 | cut -c 2- | diff foo -)
    [ -z "$thediff" ]

  '';

}
