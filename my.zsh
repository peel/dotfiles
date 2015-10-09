# functions
function gi() { curl -L -s https://www.gitignore.io/api/\$@ ;}

function ghidoing() {
  if [[ "$1" = <-> ]]
  then
    ghi label "$1" -a "in progress"
  else
    ghi open -m "$1" -L "in progress"
  fi
}

function ghidone() {
  if [[ "$1" = <-> ]]
  then
    ghi close "$1"
  else
    ghi close $(ghi open -m "$1" -L "in progress" | head -n 1 | awk 'match($0, /[0-9]+/){print substr($0, RSTART, RLENGTH)}')
  fi
}



# aliases
alias git=hub
alias e=emacs

# variables
eval $(docker-machine env docker)

export EDITOR=/usr/bin/emacs
export SHELL=/usr/local/bin/zsh
