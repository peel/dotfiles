{ config, pkgs, ... }:

{
  #services.emacs.enable = true;
  environment.systemPackages = with pkgs; [
    emacs
    aspell
    silver-searcher
    fasd
    git
    gitAndTools.hub
  ];
}
