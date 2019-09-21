{ pkgs ? import <nixpkgs> {}, ...}:

let
  haskell = pkgs.haskell-nix;

  # Import the file you will create in the stack-to-nix or cabal-to-nix step.
  my-pkgs = import ./pkgs.nix;

  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = my-pkgs;
    pkg-def-extras = [];
    modules = [];
  };

in pkgSet.config.hsPkgs // { _config = pkgSet.config; }
