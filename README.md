# xhb-keysyms

`KEYSYM` definitions for [xhb](https://hackage.haskell.org/package/xhb) scraped from [x11proto](https://cgit.freedesktop.org/xorg/proto/x11proto).

Documentation can be found [here](https://nspin.github.io/xhb-keysyms).

## Building

[Nix](https://nixos.org/nix/) is used to build this package.

- `./build-utils` contains the program that generates `Graphics.XHB.KeySym.Defs`, `Graphics.XHB.KeySym.Alph.Internal`, and `Graphics.XHB.KeySym.Names.Internal` from raw protocol documents.
- The nix expression in `./xhb-keysyms-src.nix` shows how it is used.
- That expression describes a derivation containing the entire source of the package `xhb-keysyms`.
- `./xhb-keysyms.nix` describes the package itself.
- `./default.nix` describes a `nixpkgs` that includes `haskellPackages.xhb-keysyms-src`, `haskellPackages.xhb-keysyms-build-utils`, and `haskellPackages.xhb-keysyms`.

## Developing

`./mklinks` creates symlinks in `.` to the generated source in `/nix/store` to allow you to develop normally (e.g. with `ghci`).
