{ config, pkgs, lib, ... }:

with lib; 
{
  environment.systemPackages = [ pkgs.gnupg ]
    ++ optionals pkgs.stdenvNoCC.isDarwin [ pkgs.pinentry_mac ]
    ++ optionals pkgs.stdenvNoCC.isLinux  [ pkgs.pinentry ];
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
}
