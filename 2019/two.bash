#!/bin/bash
# advent of code 2019 day 2 implementation in bash
# takes intcode as a non-spaced comma-separated list of integers

# get the intcode
IFS=','; code=($1); unset IFS

# execute
i=0
continue=1
while [[ continue -eq 1 ]]; do
	case "${code[i]}" in
		"1")
			code[${code[i+3]}]=$((${code[code[i+1]]}+${code[code[i+2]]}))
			;;
		"2")
			code[${code[i+3]}]=$((${code[code[i+1]]}*${code[code[i+2]]}))
			;;
		"99")
			continue=0
			;;
		*)
			exit 1
			;;
	esac
	i=$((i + 4))
done


# display results
echo $(IFS=","; echo "${code[*]}")
