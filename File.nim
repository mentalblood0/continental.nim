import std/syncio
import std/unittest

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
    stack: seq[Natural]

  DataKind* = enum
    dkNatural
    dkString

  DataObj = object
    case kind*: DataKind
    of dkNatural: nat*: Natural
    of dkString: str*: string

  Data = ref DataObj

func new_data(n: Natural): Data =
  Data(kind: dkNatural, nat: n)

func new_data(s: string): Data =
  Data(kind: dkString, str: s)

func `==`(a: Data, b: Data): bool =
  if a.kind != b.kind:
    return false
  case a.kind
  of dkNatural:
    a.nat == b.nat
  of dkString:
    a.str == b.str

proc new_continent*(path: string): Continent =
  new(result)
  result.path = path
  result.file = open(path, fm_read_write)

proc pos(c: Continent): Natural =
  c.file.get_file_pos

proc `pos=`(c: Continent, value: Natural) =
  c.file.set_file_pos value

proc `rpos=`(c: Continent, value: Natural) =
  c.file.set_file_pos -value, fsp_end

proc rmove(c: Continent, value: Natural) =
  c.file.set_file_pos -value, fsp_cur

proc write(c: Continent, t: DataKind) =
  do_assert c.file.write_bytes(@[uint8(t)], 0, 1) == 1

proc read_bytes(c: Continent, n: Natural): seq[uint8] =
  do_assert c.pos >= n
  c.rmove n
  result = block:
    var r: seq[uint8]
    for i in 1 .. n:
      r.add 0
    do_assert c.file.read_bytes(r, 0, r.len) == r.len
    r
  c.rmove n

proc read_chars(c: Continent, n: Natural): string =
  do_assert c.pos >= n
  c.rmove n
  result = block:
    var r: string
    for i in 1 .. n:
      r &= 'a'
    do_assert c.file.read_chars(r, 0, r.len) == r.len
    r
  c.rmove n

proc read_byte(c: Continent): uint8 =
  c.read_bytes(1)[0]

proc write*(c: Continent, i: Natural, write_type: bool = true) =
  let p = block:
    var r = i.to_seq
    r.add(uint8(r.len))
    if write_type:
      r.add(uint8(dkNatural))
    r
  do_assert c.file.write_bytes(p, 0, p.len) == p.len

proc read_natural*(c: Continent): Natural =
  to_natural c.read_bytes Natural c.read_byte

proc write*(c: Continent, s: string) =
  do_assert c.file.write_chars(s, 0, s.len) == s.len
  c.write(s.len, write_type = false)
  c.write dkString

proc read_string(c: Continent): string =
  let size = c.read_natural
  c.read_chars size

proc write*(c: Continent, d: Data) =
  case d.kind
  of dkNatural:
    c.write d.nat
  of dkString:
    c.write d.str

proc read(c: Continent): Data =
  case DataKind c.read_byte
  of dkNatural:
    Data(kind: dkNatural, nat: c.read_natural)
  of dkString:
    Data(kind: dkString, str: c.read_string)

proc array(c: Continent) =
  c.stack.add c.file.get_file_pos

proc `end`(c: Continent) =
  let begin = c.stack.pop

proc test() =
  proc test(payload: seq[Data]) =
    block:
      let c = "../test.bin".new_continent
      for p in payload:
        c.write p
      c.rpos = 0
      for i in countdown(payload.len - 1, 0):
        check payload[i] == c.read

  test @[new_data 1234, new_data "abcd"]

if is_main_module:
  test()
