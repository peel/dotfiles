{ config, pkgs, ... }:

let
  emacs = (import emacs/default.nix {inherit pkgs;});
in {
  environment.variables.EDITOR = "/run/current-system/sw/bin/emacsclient -tc";
  environment.variables.ALTERNATE_EDITOR = "/run/current-system/sw/bin/emacs";

  environment.systemPackages = [ pkgs.binutils emacs pkgs.emacs-lsp-booster];# pkgs.nodePackages.mermaid-cli ];

  services.emacs = {
    enable = true;
    package = emacs;
  };
  environment.shellAliases = {
    vim = "${emacs}/bin/emacsclient -nw";
    e   = "${emacs}/bin/emacsclient -nw";
  };
}
