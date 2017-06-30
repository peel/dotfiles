{ stdenv, config, pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    remacs
  ];
  services.emacs.enable = true;
  services.emacs.package = pkgs.remacs;
  services.emacs.exec = "remacs";
}
