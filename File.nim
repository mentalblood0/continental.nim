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

converter to_natural(bytes: seq[uint8]): Natural =
  var m = 0
  for b in bytes:
    if m == 0:
      m = 1
    else:
      m *= 256
    result += int(b) * m

type Continent = ref object
  path*: string
  file: File

proc new_continent(path: string): Continent =
  new(result)
  result.path = path
  result.file = open(path, fm_read_write)

proc write(f: Continent, i: Natural, pos: Natural) =
  f.file.set_file_pos pos
  let payload = i.to_seq
  do_assert f.file.write_bytes(@[uint8(payload.len)], 0, 1) == 1
  do_assert f.file.write_bytes(payload, 0, payload.len) == payload.len

proc read_natural(f: Continent, pos: Natural): Natural =
  f.file.set_file_pos pos
  let size = block:
    var s = @[uint8(0)]
    do_assert f.file.read_bytes(s, 0, 1) == 1
    Natural s[0]

  result = block:
    var r: seq[uint8]
    for i in 1 .. size:
      r.add 0
    do_assert f.file.read_bytes(r, 0, size) == size
    to_natural @r

proc test() =
  const path = "../test.bin"

  block test_natural:
    const n = 1234
    let c = path.new_continent
    c.write(n, 0)
    check n == c.read_natural 0

if is_main_module:
  test()
