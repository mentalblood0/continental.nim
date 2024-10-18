import std/syncio
import std/sugar
import std/paths
import std/unittest
import std/files

type IntegerEncodingError = object of RangeDefect

func to_seq(i: Natural): seq[uint8] =
  var c = i
  while true:
    result.add(uint8(c mod 256))
    c = c div 256
    if c == 0:
      break

converter to_int(bytes: seq[uint8]): BiggestInt =
  var m = 0
  for b in bytes:
    if m == 0:
      m = 1
    else:
      m *= 256
    result += int(b) * m

proc write_natural(f: File, i: Natural) =
  let payload = i.to_seq
  do_assert f.write_bytes(@[uint8(payload.len)], 0, 1) == 1
  do_assert f.write_bytes(payload, 0, payload.len) == payload.len

proc read_natural(f: File): BiggestInt =
  let size = block:
    var s = @[uint8(0)]
    do_assert f.read_bytes(s, 0, 1) == 1
    Natural s[0]

  result = block:
    var r: seq[uint8]
    for i in 1 .. size:
      r.add 0
    do_assert f.read_bytes(r, 0, size) == size
    to_int @r

const raw_path = "../test.bin"
const path = Path "../test.bin"
const n = 1234

if not file_exists path:
  remove_file path

block:
  let f = open(raw_path, fm_write)
  f.write_natural n
  close f

block:
  let f = open(raw_path, fm_read)
  check n == f.read_natural
  close f
