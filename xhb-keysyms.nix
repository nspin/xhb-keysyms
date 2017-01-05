{ mkDerivation, base, stdenv, xhb, containers, xhb-keysyms-src }:
mkDerivation {
  pname = "xhb-keysyms";
  version = xhb-keysyms-src.version;
  src = xhb-keysyms-src;
  libraryHaskellDepends = [ base xhb containers ];
  license = stdenv.lib.licenses.mit;
}
