#!/bin/bash
# advent of code 2019 day 2 solution bruteforce implementation in bash

# TODO: finish

# two_input.txt is the input intcode string provided to you with noun and verb replaced with a and b, respectively
# two_target.txt is the required result
target=$(cat two_target.txt)
input=$(cat two_input.txt)

echo -n "a=0, b=0"
for ((a=0; a<=99; a++)); do
	for ((b=0; b<=99; b++)); do
		echo -ne "\ra=$a, b=$b"
		out=$(bash two.bash $(echo "$input" | sed "s/a/$a/" | sed "s/b/$b/"))
		out=(${out//,/ })
		echo -n ", out=${out[0]}"
		if [[ "${out[0]}" == "$target" ]]; then
			echo -e "\n100 * $a + $b = $((100 * $a + $b))"
			echo "done."
			exit 0
		fi
	done
done
