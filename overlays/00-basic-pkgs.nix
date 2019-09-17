self: pkgs:

let
  sources = import <dotfiles/pinned> { inherit (pkgs) fetchgit lib; };
in {
  cachix = (import (builtins.fetchTarball "https://cachix.org/api/v1/install") { }).cachix;
  ghcide = import sources.ghcide-nix {};
  haskell-nix = import sources.haskell-nix { }; 
}







