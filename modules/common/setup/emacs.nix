{ config, lib, pkgs, ... }:

let
  cfg = config.peel.emacs;
  emacs = (import emacs/default.nix {inherit pkgs;});
  beans = pkgs.buildGoModule {
    name = "beans";
    pname = "beans";
    versions = "0.4.0";
    src = pkgs.fetchFromGitHub {
      owner = "hmans";
      repo = "beans";
      rev = "v0.4.0";
      sha256 = "sha256-3SgTqR5DGAb5r+VU3YknoWATqglq8G7QV3kTVUSL9u4=";
    };
    ldflags = ["-s" "-w"];
    vendorHash = "sha256-TprfPZ/clb7PLMAkxF0y78bCef4XarhgHlIhIPn1nQA=";
    doCheck = false;
  };

in {
  options.peel.emacs = {
    enable = lib.mkEnableOption "emacs";
    terminal = lib.mkOption {
      default = pkgs.ghostty-bin;
    };
  };
  config = lib.mkIf cfg.enable {
    environment.variables.EDITOR = "/run/current-system/sw/bin/emacsclient -tc";
    environment.variables.ALTERNATE_EDITOR = "/run/current-system/sw/bin/emacs";
    environment.systemPackages = [ pkgs.binutils emacs pkgs.emacs-lsp-booster cfg.terminal beans ];# pkgs.nodePackages.mermaid-cli ];

    services.emacs = {
      enable = true;
      package = emacs;
    };
    environment.shellAliases = {
      vim = "${emacs}/bin/emacsclient -nw";
      e   = "${emacs}/bin/emacsclient -nw";
    };
  };
}
