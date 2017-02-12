{ config, pkgs, ... }: with pkgs;

{
  users.defaultUserShell = "/run/current-system/sw/bin/fish";
  programs = {
    fish = {
      enable = true;
    };
  };
}
