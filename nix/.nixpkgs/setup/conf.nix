{ config, pkgs, ... }:

{
  programs = {
    fish = {
      enable = true;
    };
  };
  environment.loginShell = "/run/current-system/sw/bin/fish";
  services.mopidy.package = "/usr/local";
  services.mopidy.enable = true;
  services.mopidy.mediakeys.package = "/usr/local";
  services.mopidy.mediakeys.enable = true;
}
