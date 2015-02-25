# functions
function gi() { curl -L -s https://www.gitignore.io/api/\$@ ;}

# aliases
alias git=hub
alias present='scala -Dscala.color -language:_ -nowarn -i REPLesent.scala'

# variables
export DOCKER_HOST=tcp://192.168.59.103:2376
export DOCKER_CERT_PATH=/Users/peel/.boot2docker/certs/boot2docker-vm
export DOCKER_TLS_VERIFY=1
