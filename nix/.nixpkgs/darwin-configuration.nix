{ config, pkgs, ... }: with pkgs;

{
  require = [
    ./packages.nix
    ./conf.nix
    ./emacs.nix
    ./osx.nix
  ];
}