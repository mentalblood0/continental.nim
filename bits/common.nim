import std/bitops
import std/sugar
import std/unittest
import std/math

type Bits* = ref object
  buffer*: seq[uint8]
  last_size*: uint8

proc add_0*(b: var Bits) =
  if b.last_size == 8 or b.last_size == 0:
    b.buffer.add 0
    b.last_size = 1
  else:
    b.last_size += 1

proc add_1*(b: var Bits) =
  if b.last_size == 8 or b.last_size == 0:
    b.buffer.add 1
    b.last_size = 1
  else:
    b.buffer[^1].set_bit b.last_size
    b.last_size += 1

func len*(b: Bits): Natural =
  if b.buffer.len == 0:
    0
  else:
    (b.buffer.len - 1) * 8 + Natural b.last_size

func `[]`*(b: Bits, i: Natural): bool =
  b.buffer[i div 8].test_bit i mod 8

iterator items*(b: Bits): bool =
  for i in 0 ..< b.len:
    yield b[i]

iterator pairs*(b: Bits): tuple[key: int, val: bool] =
  for i in 0 ..< b.len:
    yield (key: i, val: b[i])

iterator batches*(bits: Bits, size: int): seq[bool] =
  var buffer: seq[bool]
  for b in bits:
    buffer.add b
    if buffer.len == size:
      yield buffer
      buffer = @[]

converter to_string*(bits: Bits): string =
  for b in bits:
    if b:
      result &= '1'
    else:
      result &= '0'

converter to_bits*(s: string): Bits =
  new(result)
  for c in s:
    if c == '0': result.add_0 else: result.add_1

func `[]`*(b: Bits, first: Natural, last: Natural): Bits =
  new(result)
  for i in first .. last:
    if b[i]: result.add_1 else: result.add_0

proc test*() =
  check "01001011001".to_bits[0, 2].to_string == "0100"

if is_main_module:
  test()
