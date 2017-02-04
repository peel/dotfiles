#!/usr/bin/env fish

# Example:
# $ seq 1 100 | foldl 0 '(math $_1 + $_acc)'
#   5050

function foldl --argument init lambda -d "Applies a binary operator to a start value and all elements of an collection, going left to right."
	set -e argv[1..2]
	__functional_foldl "$init" "$lambda" "" $argv
end
