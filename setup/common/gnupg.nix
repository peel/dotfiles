{ config, pkgs, lib, ... }:

with lib; 
{
  environment.systemPackages = with pkgs; [ gnupg ]
    ++ optionals stdenv.isDarwin [ pkgs.pinentry_mac ]
    ++ optionals stdenv.isLinux  [ pkgs.pinentry ];
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
}
