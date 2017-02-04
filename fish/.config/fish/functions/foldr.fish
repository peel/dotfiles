#!/usr/bin/env fish

# Example:
# $ foldr '$_acc$_1' "" a b c
#   cba

function foldr --argument lambda init -d "Applies a binary operator to all elements of this collection or iterator and a start value, going right to left. Caveats: Does not support piping!"
	set -e argv[1..2]
	__functional_foldr "$lambda" "$init" "" $argv
end
