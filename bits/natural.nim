import std/bitops
import std/unittest
import std/math

import common

func new_bits*(n: Natural): Bits =
  new(result)
  var c = n
  while true:
    result.buffer.add uint8 c mod 256
    c = c div 256
    if c == 0:
      break
  result.last_size = uint8 result.buffer[^1].fast_log2

converter to_natural*(bits: Bits): Natural =
  for i, b in bits:
    if b:
      result.set_bit i

proc test*() =
  check 1234.new_bits[0, 2].to_string == "0100"
  for n in 1 .. 2 ^ 20:
    check n.new_bits.to_natural == n
    check n.fast_log2 == n.new_bits.len

if is_main_module:
  test()
