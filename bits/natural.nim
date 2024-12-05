import std/bitops
import std/unittest
import std/math

import common

const code_size = 3
const divider = 2 ^ code_size - 1

# 000 => end
# 100 => 0
# 010 => 1
# 110 => 2
# 001 => 3
# 101 => 4
# 011 => 5
# 111 => 6

func new_bits*(n: Natural): Bits =
  new(result)
  var c = n
  while c != 0:
    let code = c mod divider + 1
    for i in 0 ..< code_size:
      if code.test_bit i:
        result.add_1
      else:
        result.add_0
    c = c div divider
  for i in 1 .. code_size:
    result.add_0

converter to_natural*(bits: Bits): Natural =
  var n = 0
  for batch in bits.batches code_size:
    let code = block:
      var r: int
      for i, b in batch:
        if b:
          r.set_bit i
      r - 1
    if code == -1:
      break
    result += code * (divider ^ n)
    n += 1

proc test*() =
  for n in 0 .. 2 ^ 10:
    check n == to_natural to_string new_bits n

if is_main_module:
  test()
