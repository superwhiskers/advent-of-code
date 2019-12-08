# advent of code 2019 day 5 implementation in nim
# takes intcode as a non-spaced comma-separated list of integers as arguments

import strutils
import os
import sequtils
import strformat

# processes modes and arguments and pairs them up
proc argModePair(modes: seq[int]; args: seq[string]): seq[tuple[arg: int, mode: int]] =
  result = @[]
  for i in 0..args.len-1:
    if i < modes.len:
      result.add((arg: parseInt(args[i]), mode: modes[modes.len - i - 1]))
    else:
      result.add((arg: parseInt(args[i]), mode: 0))

# converts a mode,arg tuple into an argument
proc convertModePairToArg(pair: tuple[arg: int, mode: int], insts: ref seq[string]): int =
  case pair.mode
  of 0:    # pointer
    var pos = pair.arg
    while pos < 0:
      pos = insts[].len + pos
    result = parseInt(insts[pos])
  of 1:    # literal
    result = pair.arg
  else:
    echo(fmt"unknown mode: {pair.mode}")

# handle missing intcode
if not declared(commandLineParams) or commandLineParams().len == 0:
  echo("no intcode was passed to the program. exiting")
  quit(1)

# nim's pointer types are disgusting
var insts: ref seq[string]
new(insts)
insts[] = commandLineParams()[0].split(',')

# main execution loop
block ExecutionLoop:
  var i = 0
  while true:
    # parse the instruction modes and the instruction itself
    var inst = parseInt(insts[i])
    var modes: seq[int]
    if insts[i].len > 2:
      inst = parseInt(insts[i][^2..insts[i].len-1])
      modes = toSeq(insts[i][0..^3].items).map(proc (i: char): int = parseInt($i))
    var modepair: seq[tuple[arg: int, mode: int]]

    # execute the instructions
    case inst
    of 1:  # c = add(a, b)
      # parse arguments
      modepair = argModePair(modes, insts[i+1..i+3])

      # perform the operation
      insts[modepair[2].arg] = $(convertModePairToArg(modepair[0], insts) + convertModePairToArg(modepair[1], insts))
    of 2:  # c = multiply(a, b)
      # parse arguments
      modepair = argModePair(modes, insts[i+1..i+3])

      # perform the operation
      insts[modepair[2].arg] = $(convertModePairToArg(modepair[0], insts) * convertModePairToArg(modepair[1], insts))
    of 3:  # a = input()
      # parse arguments
      modepair = argModePair(modes, @[insts[i+1]])

      # perform the operation
      stdout.write("input: ")
      insts[modepair[0].arg] = readline(stdin)
    of 4:  # output(a)
      # parse arguments
      modepair = argModePair(modes, @[insts[i+1]])

      # perform the operation
      echo(fmt"output: {insts[modepair[0].arg]}")
    of 5: # jump_if_true(a, b)
      # parse arguments
      modepair = argModePair(modes, insts[i+1..i+2])

      # perform the operation
      if convertModePairToArg(modepair[0], insts) != 0:
        i = convertModePairToArg(modepair[1], insts)

        # skip automatic ip incrementation
        continue
    of 6: # jump_if_false(a, b)
      # parse arguments
      modepair = argModePair(modes, insts[i+1..i+2])

      # perform the operation
      if convertModePairToArg(modepair[0], insts) == 0:
        i = convertModePairToArg(modepair[1], insts)

        # skip automatic ip incrementation
        continue
    of 7: # c = lt(a, b)
      # parse arguments
      modepair = argModePair(modes, insts[i+1..i+3])

      # perform the operation
      if convertModePairToArg(modepair[0], insts) < convertModePairToArg(modepair[1], insts):
        insts[modepair[2].arg] = "1"
      else:
        insts[modepair[2].arg] = "0"
    of 8: # c = eq(a, b)
      # parse arguments
      modepair = argModePair(modes, insts[i+1.. i+3])

      # perform the operation
      if convertModePairToArg(modepair[0], insts) == convertModePairToArg(modepair[1], insts):
        insts[modepair[2].arg] = "1"
      else:
        insts[modepair[2].arg] = "0"
    of 99: # exit(0)
      break ExecutionLoop
    else:
      echo(fmt"invalid instruction: {inst}")
      break ExecutionLoop

    # increment instruction pointer
    i += modepair.len + 1

# output the result
echo(fmt"memory={insts[]}")

