#!/usr/bin/env fish

# Example:
# $ scanr '(math $_1 + $_acc)' 0 (seq 1 5)
#   0 \n 5 \n 9 \n 12 \n 14 \n 15 \n

function scanl --argument init lambda -d "Produces a collection containing cumulative results of applying the operator going left to right."
	set -e argv[1..2]
	__functional_foldl "$init" "$lambda" 'echo $_acc' $argv
end
