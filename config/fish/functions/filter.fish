#!/usr/bin/env fish

# Example:
# $ filter '[ $_1 -gt 10 ]' 12 1 21
#   12 \n 21 \n

function filter --argument lambda -d "Executes predicate lambda and returns values for which predicate is fulfilled."
	set -e argv[1]
	__functional_impl "eval $lambda; and echo \$_1" $argv
end
