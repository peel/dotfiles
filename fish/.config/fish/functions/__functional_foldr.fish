function __functional_foldr --argument lambda init iter
	set -l _acc $init
	set -l __count (count $argv)
	while [ $__count -ge 4 ]
		eval "$iter"
		set _1 $argv[$__count]
		set _acc (eval "echo $lambda")
		set __count (math "$__count - 1")
	end
	echo $_acc
end