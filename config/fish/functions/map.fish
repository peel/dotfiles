#!/usr/bin/env fish

# Examples:
# $ map '$_1$_1' a b c
#   aa \n bb \n cc \n
#
# $ printf "%d\n" 1 2 3 | map '(math "$_1 ^ 2")'
#   1 \n 4 \n 9 \n

function map --argument lambda -d "Builds a new collection by applying a function to all elements of source collection."
	set -e argv[1]
	__functional_impl "echo $lambda" $argv
end
