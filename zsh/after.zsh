# open issues for all scala projects in directory
ghi-mass-open(){
  for project in *(e:'[[ -e $REPLY/src/main/scala ]]':); do
    ghi open "[$project] $1"
  done
}
alias "ghi opens"="ghi-mass-open"

# open an epic
# 0. create a new branch
# 1. create a [wip] pull request
# 2. create a milestone
# 3. set env variable with milestone id
alias gp=ghi_epic_push
alias ghe=ghi_epic_open
alias ghio=ghi_epic_issue_open
alias ghic=ghi_close
alias ghE=ghi_epic_close
function ghi_epic_open() {
    if [[ -n "$1" ]]
    then
        TITLE=$1
        FORMATTED=${1// /-}
        git checkout -b "f-${FORMATTED}"
        echo "switching to branch f-${FORMATTED}"
        local MILESTONE=$(ghi milestone -m "${TITLE}" | awk 'match($0, /[0-9]+/){print substr($0, RSTART, RLENGTH)}')
        ${MILESTONE} > .milestone
        echo "opened milestone ${MILESTONE} and set up env"
    else
        echo "Please provide epic title"
    fi
}
function ghi_epic_push(){
    local MILESTONE < .milestone
    local PULLREQUEST < .pullrequest
    local BRANCH = git rev-parse --abbrev-ref HEAD
    local TITLE = ${BRANCH[2,50]//-/ }
    if [[ -n $MILESTONE && ! -n $PULLREQUEST ]]; then
        git push -u origin "${BRANCH}"
        local PULLREQUEST = $(hub pull-request -m "[wip] ${TITLE}" | awk 'match($0, /[0-9]+/){print substr($0, RSTART, RLENGTH)}')
        ${PULLREQUEST} > .pullrequest
    elif [[ -n $MILESTONE && -n $PULLREQUEST ]]; then
         git push -u origin "${BRANCH}"
    else
        git push
    fi
}
ghi_epic_close(){
    local MILESTONE < .milestone
    local BRANCH = git rev-parse --abbrev-ref HEAD
    local TITLE = ${BRANCH[2,50]//-/ }
    if [[ -n $MILESTONE && ! -n $PULLREQUEST ]]; then
        git push
        ghi milestone -s 'closed' $MILESTONE
        hub pull-request -m "[wip] ${TITLE}"
        git checkout master
        git branch -d $BRANCH
    else
        git push
    fi
}
ghi_epic_issue_open() {
    local MILESTONE < .milestone
    if [[ -n $MILESTONE ]]; then
        ghi open $1 -M $MILESTONE
    else
        ghi open $1
    fi
}

ghi_doing() {
  if [[ "$1" = <-> ]]; then
    ghi label "$1" -a "in progress"
    ghi assign $1 -u $(git config user.name)
  else
    ghi open -m "$1" -L "in progress" | xargs -0 bash -c 'for id; do git checkout -b "f-issue-$id"'
    ghi assign $1 -u $(git config --global user.name)
  fi
}

ghi_close() {
  if [[ "$1" = <-> ]]; then
    ghi close "$1"
  else
    ghi close $(ghi open -m "$1" -L "in progress" | head -n 1 | awk 'match($0, /[0-9]+/){print substr($0, RSTART, RLENGTH)}')
    ghi assign $1 -u $(git config --global user.name)
  fi
}

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
