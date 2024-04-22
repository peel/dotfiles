{ config, pkgs, ... }:

let
  mkCache = url: key: { inherit url key; };
  caches =
    let
      nixos = mkCache "https://cache.nixos.org" "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
      peel = mkCache "https://peel.cachix.org" "peel.cachix.org-1:juIxrHgL76bYKcfIB/AdBUQuwkTwW5OLpPvWNuzhNrE=";
      cachix = mkCache "https://cachix.cachix.org" "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM=";
      nix-tools = mkCache "https://nix-tools.cachix.org" "nix-tools.cachix.org-1:ebBEBZLogLxcCvipq2MTvuHlP7ZRdkazFSQsbs0Px1A=";
      nix-community = mkCache "https://nix-community.cachix.org" "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
      iohk = mkCache "https://hydra.iohk.io" "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=";
      iohk-cachix = mkCache "https://iohk.cachix.org" "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=";
      emacs-darwin = mkCache "https://cachix.org/api/v1/cache/emacs" "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY=";
    in [ nixos peel cachix nix-community nix-tools ];
in {
  nix.settings.sandbox = !pkgs.targetPlatform.isx86;
  nix.settings.extra-sandbox-paths = [] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
    "/System/Library/Frameworks"
    "/System/Library/PrivateFrameworks"
    "/usr/lib"
    "/private/tmp"
    "/private/var/tmp"
    "/usr/bin/env"
  ];

  nix.settings.substituters = builtins.map (x: x.url) caches;
  nix.settings.trusted-public-keys = builtins.map (x: x.key) caches;
  nix.settings.trusted-users = [ "@admin" "root" "peel" ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreeRedistributable = true;
}
