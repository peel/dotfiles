{config, lib, pkgs, ...}:

with lib;

let
  username = "peel";
  hostName = "fff666";
in rec {
  system.stateVersion = 3;
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreeRedistributable = true;
  nixpkgs.overlays = 
    let path = <nixpkgs-overlays>; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)))
    ++ [ (import <nurpkgs-peel/overlay.nix>) ];
  nix.package = pkgs.nix;
  nix.useSandbox = false;
  nix.binaryCachePublicKeys = [ "peel.cachix.org-1:juIxrHgL76bYKcfIB/AdBUQuwkTwW5OLpPvWNuzhNrE="];
  nix.trustedBinaryCaches = [ https://peel.cachix.org ];
  nix.trustedUsers = [ "${username}" "@admin" ];
  nix.maxJobs = 4;
  nix.extraOptions = ''
    binary-caches-parallel-connections = 3
    connect-timeout = 5
  '';
  nix.nixPath = [
    "darwin-config=$HOME/.config/nixpkgs/machines/darwin/configuration.nix"
    "darwin=$HOME/.nix-defexpr/channels/darwin"
    "nixpkgs=/nix/var/nix/profiles/per-user/peel/channels/nixpkgs"
    "nixpkgs-overlays=$HOME/.config/nixpkgs/overlays"
    "nurpkgs-peel=$HOME/.config/nurpkgs"
    "setup=$HOME/.config/nixpkgs/setup"
    "$HOME/.nix-defexpr/channels"
  ];
  
  networking.hostName = hostName;

  imports = let modules = (import <nurpkgs-peel/darwin-modules>); in [
    <setup/common.nix>
    <setup/darwin.nix>
    <setup/packages.nix>
    modules.bloop
  ];

}
