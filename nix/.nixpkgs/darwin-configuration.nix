{config, lib, pkgs, ...}:

with (import libs/default.nix { inherit lib; });

let
  colors = import ./setup/colors.nix;
  fonts = import ./setup/fonts.nix;
  username = "peel";
  hostName = "fff666";
in rec {
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreeRedistributable = true;
  nixpkgs.overlays = [ (import (mkOverlay username username)) ];
  nix.package = pkgs.nixUnstable;
  nix.useSandbox = true;
  nix.maxJobs = 4;

  networking.hostName = hostName;

  require = [
    ./setup/common.nix
    ./setup/darwin.nix
    ./setup/packages.nix
    ./setup/emacs.nix
  ] ++ import (mkOverlay username "darwin-modules/module-list");

  nixpkgs.config.packageOverrides = pkgs : rec {
    alacrittyDrv = pkgs.callPackage (builtins.toPath "/Users/${username}/.config/nixpkgs/overlays/pkgs/applications/misc/alacritty") {};
    alacrittyWrapper = import ./setup/alacritty {
      inherit colors fonts;
      inherit (pkgs) stdenv makeWrapper writeTextFile;
      alacritty = alacrittyDrv;
    };
  };
}
