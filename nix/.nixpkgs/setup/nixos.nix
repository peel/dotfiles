{ config, pkgs, ... }:

let
  colors = import ./colors.nix;
  tmuxConfig = import ./tmux.nix {inherit colors;};
in {
  programs.tmux.extraTmuxConf = tmuxConfig;
  programs.tmux.shortcut = "a";
  programs.tmux.keyMode = "vi";
  programs.tmux.newSession = true;
  programs.tmux.historyLimit = 9999;
}
