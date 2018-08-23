{config, lib, pkgs, ...}:

with lib;

let
  colors = (import ../setup/colors.nix { theme = "dark"; });
  fonts = import ../setup/fonts.nix;
  username = "peel";
  hostName = "fff666";
in rec {
  system.stateVersion = 3;
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreeRedistributable = true;
  nixpkgs.overlays = [
    (import  <nurpkgs-peel>)
    (import  <conf-wrappers> { inherit colors fonts; })
    (import ../setup/envs.nix)
  ];
  nix.package = pkgs.nix;
  nix.useSandbox = true;
  nix.binaryCachePublicKeys = [ "peel.cachix.org-1:juIxrHgL76bYKcfIB/AdBUQuwkTwW5OLpPvWNuzhNrE="];
  nix.trustedBinaryCaches = [ https://peel.cachix.org ];
  nix.maxJobs = 4;
  nix.extraOptions = ''
    binary-caches-parallel-connections = 3
    connect-timeout = 5
  '';
  nix.nixPath = [
    "darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix"
    "darwin=$HOME/.nix-defexpr/channels/darwin"
    "nixpkgs=/nix/var/nix/profiles/per-user/peel/channels/nixpkgs"
    "nurpkgs-peel=$HOME/.config/nurpkgs/overlay.nix"
    "conf-wrappers=$HOME/.config/nixpkgs/config-wrappers/default.nix"
    "$HOME/.nix-defexpr/channels"
  ];
  
  networking.hostName = hostName;

  imports = [
    ../setup/common.nix
    ../setup/darwin.nix
    ../setup/packages.nix
  ];

}
