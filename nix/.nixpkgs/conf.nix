{ config, pkgs, ... }:

{
  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
    allowUnfreeRedistributable = true;
  };
  # users.defaultUserShell = "/run/current-system/sw/bin/fish";
  programs = {
    fish = {
      enable = true;
    };
  };
}
