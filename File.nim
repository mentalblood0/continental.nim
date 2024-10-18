import std/syncio
import std/options
import std/with
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

type Continent* = ref object
  path*: string
  file: File

proc new_continent*(path: string): Continent =
  new(result)
  result.path = path
  result.file = open(path, fm_read_write)

proc `pos=`(c: Continent, value: int) =
  c.file.set_file_pos value

proc read_size(f: Continent): Natural =
  var s = @[uint8(0)]
  do_assert f.file.read_bytes(s, 0, 1) == 1
  return Natural s[0]

proc write*(c: Continent, i: Natural) =
  let payload = i.to_seq
  do_assert c.file.write_bytes(@[uint8(payload.len)], 0, 1) == 1
  do_assert c.file.write_bytes(payload, 0, payload.len) == payload.len

proc read_natural*(c: Continent): Natural =
  let size = c.read_size

  var r: seq[uint8]
  for i in 1 .. size:
    r.add 0
  do_assert c.file.read_bytes(r, 0, size) == size
  return to_natural @r

proc write*(c: Continent, s: string) =
  c.write s.len
  do_assert c.file.write_chars(s, 0, s.len) == s.len

proc read_string*(c: Continent): string =
  let size = c.read_natural

  var r: string
  for i in 1 .. size:
    r.add char(0)
  do_assert c.file.read_chars(r, 0, size) == size
  return r

proc test() =
  template test(read_f: untyped, payload: untyped) =
    block:
      let c = "../test.bin".new_continent
      c.write payload
      c.pos = 0
      check payload == c.read_f

  read_natural.test 1234
  read_string.test "abcd"

if is_main_module:
  test()
