{ config, pkgs, ... }:

let
  colors = import ./colors.nix;
  tmuxConfig = import ./tmux.nix {inherit colors; inherit (pkgs) tmux-prompt; };
in {
  programs.tmux.extraTmuxConf = tmuxConfig;
  programs.tmux.shortcut = "a";
  programs.tmux.newSession = true;
  programs.tmux.historyLimit = 9999;
}
