{ config, pkgs, ...}:

{
  windowManager = {
    default = "xmonad";
    xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };
    
  system.userActivationScripts.xmonad.text = ''
    ln -sfn ${<dotfiles/overlays/20-xmonad/xmonad>} $HOME/.xmonad
  '';
}
