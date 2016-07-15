{ mkDerivation, attoparsec, base, haskell-src-exts, stdenv, text }:
mkDerivation {
  pname = "xhb-keysyms-build-utils";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base haskell-src-exts text
  ];
  license = stdenv.lib.licenses.mit;
}
