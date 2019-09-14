{ config, pkgs, headless, ... }:

let
  mkCache = url: key: { inherit url key; };
  caches =
    let
      nixos = mkCache "https://cache.nixos.org" "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
      peel = mkCache "https://peel.cachix.org" "peel.cachix.org-1:juIxrHgL76bYKcfIB/AdBUQuwkTwW5OLpPvWNuzhNrE=";
      cachix = mkCache "https://cachix.cachix.org" "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM=";
    in [ nixos peel cachix ];
in {
  nix.useSandbox = true;
  nix.sandboxPaths = [] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
    "/System/Library/Frameworks"
    "/System/Library/PrivateFrameworks"
    "/usr/lib"
    "/private/tmp"
    "/private/var/tmp"
    "/usr/bin/env"
  ];
  
  nix.binaryCaches = builtins.map (x: x.url) caches;
  nix.binaryCachePublicKeys = builtins.map (x: x.key) caches;
  nix.trustedUsers = [ "@admin" "root" ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreeRedistributable = true;

  environment.systemPackages = with pkgs; [
    # cachix
    coreutils
    gnupg
    htop
    ripgrep
    scripts
  ];
}
