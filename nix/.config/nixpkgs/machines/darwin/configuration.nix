{config, lib, pkgs, ...}:

with lib;

let
  sources = import <setup/pinned> { inherit (pkgs) lib; };
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
  nix.package = lib.toDerivation /nix/store/dkjlfkrknmxbjmpfk3dg4q3nmb7m3zvk-nix-2.1.3 // { version = "2.1.3"; };
  nix.useSandbox = false;
  nix.binaryCachePublicKeys = [ "peel.cachix.org-1:juIxrHgL76bYKcfIB/AdBUQuwkTwW5OLpPvWNuzhNrE="];
  nix.trustedBinaryCaches = [ https://peel.cachix.org ];
  nix.trustedUsers = [ "${username}" "@admin" ];
  nix.maxJobs = 4;
  nix.extraOptions = ''
    binary-caches-parallel-connections = 3
    connect-timeout = 5
  '';
  environment.darwinConfig = "$HOME/.config/nixpkgs/machines/darwin/configuration.nix";
  nix.nixPath = [
    "darwin-config=$HOME/.config/nixpkgs/machines/darwin/configuration.nix"
    "darwin=${sources."nix-darwin"}"
    "nixpkgs=${sources.nixpkgs}"
    "nixpkgs-overlays=$HOME/.config/nixpkgs/overlays"
    "nurpkgs-peel=$HOME/.config/nurpkgs"
    "setup=$HOME/.config/nixpkgs/setup"
    "$HOME/.nix-defexpr/channels"
    "$HOME/.nix-defexpr"
  ];
  
  networking.hostName = hostName;

  imports = let modules = (import <nurpkgs-peel/darwin-modules>); in [
    <setup/common.nix>
    <setup/darwin.nix>
    <setup/packages.nix>
    modules.bloop
  ];

}
