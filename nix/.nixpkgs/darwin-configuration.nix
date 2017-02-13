{ config, pkgs, ... }: 

{
  require = [
    ./packages.nix
    ./conf.nix
    ./emacs.nix
    ./osx.nix
  ];
}