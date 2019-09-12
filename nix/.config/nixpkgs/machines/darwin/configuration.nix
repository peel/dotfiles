{config, lib, pkgs, ...}:

with lib;

let
  sources = import <dotfiles/setup/pinned> { inherit (pkgs) fetchgit lib; };
  username = "peel";
  hostName = "fff666";
in rec {
  nixpkgs.overlays = 
    let path = <dotfiles/overlays>; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)))
      ++ [ (import <nurpkgs-peel/overlay.nix>) ];

  networking.hostName = hostName;
  
  environment.darwinConfig = <dotfiles/machines/darwin/configuration.nix>;
  system.stateVersion = 3;
  
  nix.nixPath = [
    "darwin-config=${environment.darwinConfig}"
    "darwin=${sources."nix-darwin"}"
    "nixpkgs=${sources.nixpkgs}"
    "dotfiles=$HOME/.config/nixpkgs"
    "nurpkgs-peel=$HOME/.config/nurpkgs"
    "$HOME/.nix-defexpr/channels"
    "$HOME/.nix-defexpr"
  ];

  imports = let modules = (import <nurpkgs-peel/darwin-modules>); in [
    modules.bloop
    modules.yabai
    modules.weechat
    <dotfiles/setup/common.nix>
    <dotfiles/setup/darwin.nix>
    <dotfiles/setup/packages.nix>
  ];

}
