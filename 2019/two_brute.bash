#!/bin/bash
# advent of code 2019 day 2 solution bruteforce implementation in bash

# TODO: finish

# two_input.txt is the input intcode string provided to you
# two_target.txt is the required result
target=$(cat two_target.txt)
input=$(cat two_input.txt)

echo -n "a=0, b=0"
for ((a=0; a<100; a++)); do
	for ((b=0; b<100; b++)); do
		echo -ne "\ra=$a, b=$b"
		IFS=','; out=($(./two.bash $(echo "$target" | sed 's/a/$a/' | sed 's/b/$b/'))); unset IFS
		if [[ ${out[0]} -eq $target ]]; then
			echo -e "\ndone."
			exit 0
		fi
	done
done
