{ config, pkgs, ... }:

{
  environment.variables.EDITOR = "${pkgs.emacs}/bin/emacsclient -tc";
  environment.variables.ALTERNATE_EDITOR = "${pkgs.emacs}/bin/emacs";

  environment.systemPackages = [ pkgs.emacs ];
  
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };
  environment.shellAliases = {
    vim = "${pkgs.emacs}/bin/emacsclient -nw";
    e   = "${pkgs.emacs}/bin/emacsclient -nw";
  };
}
