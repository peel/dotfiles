#!/usr/bin/env fish

# Example:
# $ each 'curl $_1 > $_1.html' "github.com" "codearsonist.com"
# $ ls
# github.com.html codearsonist.com.html

function each --argument lambda -d "Apply the given procedure lambda to the option's value, if it is nonempty. Otherwise, do nothing."
	set -e argv[1]
	__functional_impl $lambda $argv
end
