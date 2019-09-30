#! /usr/bin/env nix-shell
#! nix-shell -i bash -p _1password jq

set -euo pipefail

f=$(mktemp)
trap "rm $f" EXIT
eval $(op signin my)
op get item "$1" | jq -c '.details.fields[] | select(.designation=="password") | .value' > $f
nix-instantiate --eval -E "builtins.readFile $f"
