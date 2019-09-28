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
    ./shells.nix
    ./tmux.nix
  ];

  time.timeZone = "Europe/Warsaw";
}
