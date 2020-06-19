{config, lib, pkgs, ...}:

with lib;

let
  sources = import <dotfiles/pinned> { inherit (pkgs) fetchgit lib; };
  username = "orther";
  hostName = "arson";
in rec {
  # TODO no
  nixpkgs.overlays =
    let path = <dotfiles/overlays>; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)))
      ++ [ (import <nurpkgs-orther/overlay.nix>) ];

  # TODO nay
  networking.hostName = hostName;

  nix.maxJobs = lib.mkDefault 12;

  #FIXME
  environment.darwinConfig = <dotfiles/machines/arson/configuration.nix>;
  system.stateVersion = 3;

  nix.nixPath = [
    "darwin-config=${environment.darwinConfig}"
    "darwin=${sources."nix-darwin"}"
    "nixpkgs=channel:nixos-20.03"
    "dotfiles=$HOME/.config/nixpkgs"
    "nurpkgs-orther=${sources.nurpkgs}"
    "$HOME/.nix-defexpr/channels"
    "$HOME/.nix-defexpr"
  ];

  imports = let modules = (import <nurpkgs-orther/darwin-modules>); in [
    modules.bloop
    modules.yabai
    #modules.weechat
    <dotfiles/setup/darwin>
  ];

}
