import std/syncio
import std/sugar
import std/paths
import std/unittest
import std/files

type IntegerEncodingError = object of RangeDefect

func to_seq(i: int, length: Natural): seq[uint8] =
  var c = i
  while true:
    if Natural(len(result)) == length:
      raise new_exception(IntegerEncodingError, "size of " & $i & " exceeds " &
          $length & " bytes")
    result.add(uint8(c mod 256))
    c = c div 256
    if c == 0:
      break

  let zeros = collect:
    for j in 1..length-len(result): uint8(0)
  result &= zeros

converter to_int(bytes: seq[uint8]): BiggestInt =
  var m = 0
  for b in bytes:
    if m == 0:
      m = 1
    else:
      m *= 256
    result += int(b) * m

const raw_path = "../test.bin"
const path = Path "../test.bin"
const n = 1234

if not file_exists path:
  remove_file path

proc write(f: File, i: Natural, pos: Natural, size: Natural) =
  let payload = n.to_seq size
  do_assert f.write_bytes(payload, pos, size) == size

proc read(f: File, pos: Natural, size: Natural): BiggestInt =
  var r: seq[uint8]
  for i in 1 .. size:
    r.add(0)
  do_assert f.read_bytes(r, 0, size) == size
  return to_int(@r)

block:
  let f = open(raw_path, fm_write)
  f.write(n, 0, 8)
  close f

block:
  let f = open(raw_path, fm_read)
  check n == f.read(0, 8)
  close f

