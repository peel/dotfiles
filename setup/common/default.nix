{ config, pkgs, ... }:

{
  imports = [
    ./direnv.nix
    ./emacs.nix
    ./fonts.nix
    ./git.nix
    ./gnupg.nix
    ./languages.nix
    ./packages.nix
    ./secrets
    ./shells.nix
  ];

  peel.secrets.enable = true;

  time.timeZone = "Europe/Warsaw";
}
