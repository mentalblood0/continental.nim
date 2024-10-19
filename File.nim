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

type
  Continent* = ref object
    path*: string
    file: File

  DataKind* = enum
    dkNatural
    dkString

  DataObj = object
    case kind*: DataKind
    of dkNatural: nat*: Natural
    of dkString: str*: string

  Data = ref DataObj

proc new_continent*(path: string): Continent =
  new(result)
  result.path = path
  result.file = open(path, fm_read_write)

proc `pos=`(c: Continent, value: int) =
  c.file.set_file_pos value

proc write(c: Continent, t: DataKind) =
  do_assert c.file.write_bytes(@[uint8(t)], 0, 1) == 1

proc write*(c: Continent, i: Natural, write_type: bool = true) =
  let p = block:
    let a = i.to_seq
    var r: seq[uint8]
    if write_type:
      r.add(uint8(dkNatural))
    r & @[uint8(a.len)] & a
  do_assert c.file.write_bytes(p, 0, p.len) == p.len

proc read_natural*(c: Continent): Natural =
  let size = Natural c.file.read_char

  var r: seq[uint8]
  for i in 1 .. size:
    r.add 0
  do_assert c.file.read_bytes(r, 0, size) == size
  return to_natural @r

proc write*(c: Continent, s: string) =
  c.write dkString
  c.write(s.len, write_type = false)
  do_assert c.file.write_chars(s, 0, s.len) == s.len

proc read_string(c: Continent): string =
  let size = c.read_natural

  var r: string
  for i in 1 .. size:
    r.add char(0)
  do_assert c.file.read_chars(r, 0, size) == size
  return r

proc read(c: Continent): Data =
  case DataKind c.file.read_char
  of dkNatural:
    Data(kind: dkNatural, nat: c.read_natural)
  of dkString:
    Data(kind: dkString, str: c.read_string)

proc test() =
  template test(payload: untyped, k: untyped) =
    block:
      let c = "../test.bin".new_continent
      c.write payload
      c.pos = 0
      check payload == c.read.k

  1234.test nat
  "abcd".test str

if is_main_module:
  test()
