{config, lib, pkgs, ...}:

with lib;

let
  sources = import <dotfiles/setup/pinned> { inherit (pkgs) fetchgit lib; };
  username = "peel";
  hostName = "fff666";
in rec {
  system.stateVersion = 3;
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreeRedistributable = true;
  nixpkgs.overlays = 
    let path = <dotfiles/overlays>; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)))
    ++ [ (import <nurpkgs-peel/overlay.nix>) ];
  nix.package = lib.toDerivation /nix/store/dkjlfkrknmxbjmpfk3dg4q3nmb7m3zvk-nix-2.1.3 // { version = "2.1.3"; };
  nix.useSandbox = false;
  nix.binaryCachePublicKeys = [ "peel.cachix.org-1:juIxrHgL76bYKcfIB/AdBUQuwkTwW5OLpPvWNuzhNrE="];
  nix.trustedBinaryCaches = [ https://peel.cachix.org ];
  nix.trustedUsers = [ "${username}" "@admin" ];
  environment.darwinConfig = <dotfiles/machines/darwin/configuration.nix>;
  nix.nixPath = [
    "darwin-config=${environment.darwinConfig}"
    "darwin=${sources."nix-darwin"}"
    "nixpkgs=${sources.nixpkgs}"
    "dotfiles=$HOME/.config/nixpkgs"
    "nurpkgs-peel=$HOME/.config/nurpkgs"
    "$HOME/.nix-defexpr/channels"
    "$HOME/.nix-defexpr"
  ];
  
  networking.hostName = hostName;

  imports = let modules = (import <nurpkgs-peel/darwin-modules>); in [
    <dotfiles/setup/common.nix>
    <dotfiles/setup/darwin.nix>
    <dotfiles/setup/packages.nix>
    modules.bloop
  ];

}
