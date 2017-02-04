# global env
set LC_ALL en_US.UTF-8
set LANG en_US.UTF-8
set TERM xterm-256color
set SHELL /usr/local/bin/fish
set EDITOR "emacsclient -tc"
set -U fish $HOME/.config/fish

# aliases
each 'source $_1' (ls $fish/aliases/*)

# erlang
set PATH $PATH $HOME/.mix/escripts/

# homebrew
set HOMEBREW_NO_ANALYTICS 1
set HOMEBREW_CASK_OPTS "--appdir=~/Applications --caskroom=/usr/local/Caskroom"
