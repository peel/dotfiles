#load custom functions
FUNCTIONS_DIR=$HOME/.zsh.after/functions
ALIASES_DIR=$HOME/.zsh.after/aliases
fpath=($FUNCTIONS_DIR $fpath)
autoload $FUNCTIONS_DIR/*(:t)

source $ALIASES_DIR/*

export EDITOR="emacsclient -tc"
export SHELL=/usr/local/bin/zsh
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_CASK_OPTS="--appdir=~/Applications --caskroom=/usr/local/Caskroom"

#autoload -Uz promptinit
#promptinit
prompt sorin
PS1="$PS1"'$([ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#D" | tr -d %) "$PWD")'
PATH=$PATH:~/.mix/escripts/ 
