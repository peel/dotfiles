{ stdenv, config, pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    emacs
  ];
  services.emacs.enable = true;
}
