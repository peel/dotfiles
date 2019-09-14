{ config, pkgs, ... }:

{
  imports = [
    ./direnv.nix
    ./emacs.nix
    ./fonts.nix
    ./git.nix
    ./languages.nix
    ./packages.nix    
    ./shells.nix
    ./tmux.nix
  ];

  time.timeZone = "Europe/Warsaw";
  
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  networking.dns = ["1.1.1.1" "1.0.0.1" "2606:4700:4700::1111" "2606:4700:4700::1001"];

}
