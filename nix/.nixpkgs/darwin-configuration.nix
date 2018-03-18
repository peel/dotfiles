{config, lib, pkgs, ...}:

with (import libs/default.nix { inherit lib; });

let
  colors = import ./setup/colors.nix;
  fonts = import ./setup/fonts.nix;
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
  ] ++ import (mkOverlay username "darwin-modules/module-list");

  nixpkgs.config.packageOverrides = pkgs : rec {
    alacritty = import ./setup/alacritty { inherit pkgs colors fonts; };
  };
}
