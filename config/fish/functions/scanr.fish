#!/usr/bin/env fish

# Example:
# $ scanr '(math $_1 + $_acc)' 0 (seq 1 5)
#   0 \n 5 \n 9 \n 12 \n 14 \n 15 \n

function scanr --argument lambda init -d "Produces a collection containing cumulative results of applying the operator going right to left. The head of the collection is the last cumulative result. Does not support piping!"
	set -e argv[1..2]
	__functional_foldr "$lambda" "$init" 'echo $_acc' $argv
end
