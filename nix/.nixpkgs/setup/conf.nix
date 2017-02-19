{ config, pkgs, ... }:

{
  programs = {
    fish = {
      enable = true;
    };
  };
  environment.loginShell = "/run/current-system/sw/bin/fish";
}
