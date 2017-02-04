function __functional_foldl --argument init lambda iter
	set -e argv[1..3]
	set -l _acc $init
	if [ (count $argv) -eq 0 ]
		while read -l _1
			eval "$iter"
			set _acc (eval "echo $lambda")
		end
	else
		for _1 in $argv
			eval "$iter"
			set _acc (eval "echo $lambda")
		end
	end
	echo $_acc
end
