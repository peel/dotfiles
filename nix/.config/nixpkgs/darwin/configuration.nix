{config, lib, pkgs, ...}:

with lib;

let
  colors = (import ../setup/colors.nix { theme = "dark"; });
  fonts = import ../setup/fonts.nix;
  username = "peel";
  hostName = "fff666";
  nur = (import ~/.config/nurpkgs);
in rec {
  system.stateVersion = 3;
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreeRedistributable = true;
  nixpkgs.overlays = [ (import ~/.config/nurpkgs/overlay.nix) ];
  nix.package = pkgs.nix;
  nix.useSandbox = true;
  nix.binaryCachePublicKeys = [ "peel.cachix.org-1:juIxrHgL76bYKcfIB/AdBUQuwkTwW5OLpPvWNuzhNrE="];
  nix.trustedBinaryCaches = [ https://peel.cachix.org ];
  nix.maxJobs = 4;
  nix.nixPath = [
    "darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix"
    "darwin=$HOME/.nix-defexpr/channels/darwin"
    "nixpkgs=/nix/var/nix/profiles/per-user/peel/channels/nixpkgs"
    "$HOME/.nix-defexpr/channels"
  ];
  
  networking.hostName = hostName;

  imports = [
    ../setup/common.nix
    ../setup/darwin.nix
    ../setup/packages.nix
    ../setup/emacs.nix
  ];

  nixpkgs.config.packageOverrides = pkgs : rec {
    alacrittyWrapper = import ../setup/alacritty {
      inherit colors fonts;
      inherit (pkgs) stdenv makeWrapper writeTextFile;
      alacritty = pkgs.alacritty;
    };
  };
}
