function __functional_impl --argument to_eval
	if [ (count $argv) -eq 1 ]
		while read -l _1
			eval "$to_eval"
		end
	else
		for _1 in $argv[2..-1]
			eval "$to_eval"
		end
	end
end

