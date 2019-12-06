# advent of code 2019 day 5 implementation in nim
# takes intcode as a non-spaced comma-separated list of integers as arguments

import strutils
import os
import sequtils
import strformat

proc parseArgs(modes: seq[int]; args: seq[int]; insts: ref seq[int]): seq[int] =
  result = @[]
  for i, m in modes:
    if m == 0: # pointer
      result.insert(0, insts[args[i+(args.len-i)]])
    elif m == 1: # literal
      result.insert(0, args[i+(args.len-i)])
    else:
      echo("invalid mode: {m}")


when not declared(commandLineParams):
  echo("no intcode was passed to the program. exiting")

# nim's pointer types are disgusting
var insts: ref seq[string]
new(insts)
insts = commandLineParams()[0].split(',')


block ExecutionLoop:
  for i, g in insts:
    # parse the instruction modes and the instruction itself
    let inst = parseInt(g[^2..g.len])
    let modes = g[0..^2].split("").map(proc (i: string): int = parseInt(i))
    var args: seq[int]

    # execute the instructions
    case inst
    of 1:  # c = add(a, b)
      # parse arguments
      args = parseArgs(modes, insts[i+1..i+3], new())

      # fail if the third argument is in literal mode
      if modes[0] == 1:
        echo("add: third argument cannot be in literal mode")

      # perform the operation
      insts[args[2]] = $(args[0] + args[1])
    of 2:  # c = multiply(a, b)
      # parse arguments
      args = parseArgs(modes, insts[i+1..i+3], ref insts)

      # fail if the third argument is in literal mode
      if modes[0] == 1:
        echo("multiply: third argument cannot be in literal mode")

      # perform the operation
      insts[args[2]] = $(args[0] * args[1])
    of 3:  # a = input()
      # parse arguments
      args = parseArgs(modes, @[insts[i+1]], ref insts)

      # fail if the only argument is in literal mode
      if modes[0] == 1:
        echo("input: first argument cannot be in literal mode")

      # perform the operation
      insts[args[0]] = readline(stdin)
    of 4:  # output(a)
      # parse arguments
      args = parseArgs(modes, @[insts[i+1]], ref insts)

      # fail if the only argument is in literal mode
      if modes[0] == 1:
        echo("output: first argument cannot be in literal mode")

      # perform the operation
      echo(insts[args[0]])
    of 99: # exit(0)
      break ExecutionLoop
    else:
      echo(fmt"invalid instruction: {inst}")
      break ExecutionLoop

    # increment instruction pointer
    i += args.len + 1

# output the result
echo(&"output=[{insts}]")

