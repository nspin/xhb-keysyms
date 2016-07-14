{ mkDerivation, base, stdenv, xhb }:
mkDerivation {
  pname = "xhb-keysyms";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [ base xhb ];
  license = stdenv.lib.licenses.mit;
}
