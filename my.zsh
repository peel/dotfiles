# functions
gi() { curl -L -s https://www.gitignore.io/api/\$@ ;}

ghidoing() {
  if [[ "$1" = <-> ]]
  then
    ghi label "$1" -a "in progress"
    ghi assign $1 -u $(git config user.name)
    #git checkout -b "f-issue-$1"
  else
    ghi open -m "$1" -L "in progress" | xargs -0 bash -c 'for id; do git checkout -b "f-issue-$id"'
    ghi assign $1 -u $(git config --global user.name)
  fi
}

ghidone() {
  if [[ "$1" = <-> ]]
  then
    ghi close "$1"
    #git merge "f-issue-$1" develop
  else
    ghi close $(ghi open -m "$1" -L "in progress" | head -n 1 | awk 'match($0, /[0-9]+/){print substr($0, RSTART, RLENGTH)}')
    ghi assign $1 -u $(git config --global user.name)
    #git merge "f-issue-$1" develop
  fi
}



# aliases
alias git=hub
alias e=emacs
alias docker-up="docker-machine start docker && eval $(docker-machine env docker)"

# variables
eval $(docker-machine env docker)

export EDITOR=/usr/bin/emacs
export SHELL=/usr/local/bin/zsh
