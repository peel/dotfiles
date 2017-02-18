{ config, pkgs, ... }:

{
  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
    allowUnfreeRedistributable = true;
  };

  programs = {
    fish = {
      enable = true;
    };
  };
  environment.loginShell = "/run/current-system/sw/bin/fish";
}
