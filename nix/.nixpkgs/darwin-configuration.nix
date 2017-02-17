{ config, pkgs, ... }: 

{
  nixpkgs.config.allowBroken = true;
  nixpkgs.config.allowUnfree = true;
  require = [
    ./packages.nix
    ./conf.nix
    ./emacs.nix
  ];
}