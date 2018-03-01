{config, lib, pkgs, ...}:

with (import libs/default.nix { inherit lib; });

let
    username = "peel";
    hostName = "fff666";
in {
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;
  nixpkgs.config.allowUnfreeRedistributable = true;
  nixpkgs.overlays = [ (import (mkOverlay username username)) ];
  nix.package = pkgs.nixUnstable;
  nix.useSandbox = true;

  networking.hostName = hostName;

  require = [
    ./setup/common.nix
    ./setup/darwin.nix
    ./setup/packages.nix
    ./setup/emacs.nix
  ];
}
