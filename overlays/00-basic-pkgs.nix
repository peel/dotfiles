self: pkgs:

let
  sources = import <dotfiles/pinned> { inherit (pkgs) fetchgit lib; };
in {
  ghcide = import sources."ghcide-nix" {};
  haskell-nix = import sources."haskell-nix" { inherit pkgs; };
}







