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
    ./tmux.nix
  ];

  peel.secrets.enable = true;

  time.timeZone = "America/Los_Angeles";
}
