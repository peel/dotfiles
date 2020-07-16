self: pkgs:

let
  sources = import <dotfiles/pinned> { inherit (pkgs) fetchgit lib; };
in {
  haskell-nix = import sources."haskell-nix" {};
}







