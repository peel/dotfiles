#load custom functions
FUNCTIONS_DIR=$HOME/.zsh.after/functions
fpath=($FUNCTIONS_DIR $fpath)
autoload $FUNCTIONS_DIR/*(:t)

#git workflow aliases
alias gp=ghi_epic_push
alias ghe=ghi_epic_open
alias ghios=ghi_opens
alias ghio=ghi_epic_issue_open
alias ghic=ghi_close
alias ghE=ghi_epic_close

# aliases
alias find-ips="nmap -sP $(ipconfig getifaddr en0)/24"
alias e="emacsclient -tc"
alias r="fc -ln -1"
alias R="fc -ln -1"

#docker
alias dm="docker-machine"
alias dmi="dm ip"
alias dme="dm env"
alias dms="dm start && dme"
alias dmR="dm rm default"
alias dmc="dm create --driver virtualbox default"
alias dc="docker-compose"
alias dcu="dc up"
alias dck="dc kill"
alias dcR="dc rm -f"
alias dckR="dck && dcR"
alias d="docker"
alias ds="d start"
alias dS="d stop"
alias dr="d restart"
alias dl="d log"
alias di="d images"
alias diRu="d rmi $(docker images | grep '^<none>' | awk '{print $3}')"
alias diR="d rmi -f $(di -aq)"
alias dps="d ps"
alias dex="d exec"

#kubernetes
alias k8="kubectl --context nwtprod --namespace auth"
alias k8g="k8 get pod"
alias k8l="k8 logs"
alias k8lf="k8l -f"
alias k8e="k8 exec -it"
k8G() {
    k8g | awk -v service="${1}-[0-9]+-.*" '$0 ~ service { print $1 }'
}
k8E() {
    k8e $(k8G "$1") "$2"
}
k8L() {
    k8lf $(k8G "$1")
}

#elixir
alias ni="npm install"
alias nib="ni && node node_modules/brunch/bin/brunch build"
alias ism="iex -S mix"
alias mdg="mix deps.get"
alias mpn="mix phoenix.new"
alias mps="mix phoenix.server"
alias ismps="iex -S mix phoenix.server"
alias mec="mix ecto.create"

export EDITOR="emacsclient -tc"
export SHELL=/usr/local/bin/zsh
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_CASK_OPTS="--appdir=~/Applications --caskroom=/usr/local/Caskroom"

#autoload -Uz promptinit
#promptinit
prompt sorin
PS1="$PS1"'$([ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#D" | tr -d %) "$PWD")'
PATH=$PATH:~/.mix/escripts/ 
