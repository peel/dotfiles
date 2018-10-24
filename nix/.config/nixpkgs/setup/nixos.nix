{ config, pkgs, colors, ... }:

let
  tmuxConfig = import ./tmux.nix {inherit colors; inherit (pkgs) tmux-prompt; };
in {
  programs.tmux.extraTmuxConf = tmuxConfig;
  programs.tmux.shortcut = "a";
  programs.tmux.newSession = true;
  programs.tmux.historyLimit = 9999;
  fonts = {
    fontconfig.enable = true;
    fontconfig.dpi = 180;
  };

}
