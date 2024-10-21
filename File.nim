import std/syncio
import std/options
import std/with
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
    dkArray
    dkStaticLink
    dkDynamicLink

  Path* = ref object
    c: Continent
    parent: Option[Path]
    pos: Natural

  DynamicLink = tuple[path: seq[Natural]]

  StaticLink = ref object
    c: Continent
    pos: Natural

  DataObj = object
    case kind*: DataKind
    of dkNatural: nat*: Natural
    of dkString: str*: string
    of dkArray:
      len*: Natural
      link_size: Natural
    of dkStaticLink: pos*: Natural
    of dkDynamicLink: dl*: DynamicLink

  Data = ref DataObj

  NotSupported* = object of ValueError

func new_path(c: Continent): Path =
  new(result)
  result.c = c

func new_link(path: seq[Natural]): DynamicLink = (path: path)

proc `[]`*(p: Path, i: int64): Path
proc compile(c: Continent, l: DynamicLink): StaticLink =
  new(result)
  result.c = c
  var path = c.new_path
  for n in l.path:
    path = path[n]
  result.pos = path.pos

func new_data*(n: Natural): Data =
  Data(kind: dkNatural, nat: n)

func new_data*(s: string): Data =
  Data(kind: dkString, str: s)

func `==`*(a: Data, b: Data): bool =
  if a.kind != b.kind:
    return false
  case a.kind
  of dkNatural:
    a.nat == b.nat
  of dkString:
    a.str == b.str
  else:
    raise new_exception(ValueError, "Comparing data of kind " & $a.kind & " not supported")

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

proc move(c: Continent, value: Natural) =
  c.file.set_file_pos value, fsp_cur

proc rmove(c: Continent, value: Natural) =
  c.file.set_file_pos -value, fsp_cur

proc write(c: Continent, t: DataKind) =
  do_assert c.file.write_bytes(@[uint8(t)], 0, 1) == 1

proc read_bytes(c: Continent, n: Natural): seq[uint8] =
  do_assert c.pos >= n
  c.rmove n
  result = block:
    var r: seq[uint8]
    r.set_len(n)
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
    do_assert c.file.read_chars(r) == r.len
    r
  c.rmove n

proc read_byte(c: Continent): uint8 =
  c.read_bytes(1)[0]

proc write_bytes(c: Continent, b: seq[uint8]) =
  do_assert c.file.write_bytes(b, 0, b.len) == b.len

proc write*(c: Continent, i: Natural, write_type: bool = true) =
  let p = block:
    var r = i.to_seq
    r.add(uint8(r.len))
    if write_type:
      r.add(uint8(dkNatural))
    r
  c.write_bytes p

proc read_natural(c: Continent): Natural =
  to_natural c.read_bytes Natural c.read_byte

proc write*(c: Continent, s: string) =
  do_assert c.file.write_chars(s, 0, s.len) == s.len
  c.write(s.len, write_type = false)
  c.write dkString

proc write*(c: Continent, d: Data) =
  case d.kind
  of dkNatural:
    c.write d.nat
  of dkString:
    c.write d.str
  else:
    raise new_exception(ValueError, "Direct writing of data of kind " &
        $d.kind & " not supported")

proc go_link(c: Continent, size: Natural) =
  let l = c.read_bytes size
  c.pos = to_natural l

proc read(c: Continent): Data =
  let t = DataKind c.read_byte
  case t
  of dkNatural:
    Data(kind: dkNatural, nat: c.read_natural)
  of dkString:
    Data(kind: dkString, str: c.read_chars c.read_natural)
  of dkArray:
    let ls = Natural c.read_byte
    let l = c.read_natural
    Data(kind: dkArray, len: l, link_size: ls)
  of dkStaticLink:
    c.go_link Natural c.read_byte
    c.read
  of dkDynamicLink:
    raise new_exception(ValueError, "Dynamic links not supported yet")

proc move_to_element(c: Continent, a: Data, i: int64) =
  do_assert a.kind == dkArray
  let ni = block:
    if i >= 0:
      i
    else:
      a.len + i
  if ni >= a.len:
    raise new_exception(IndexDefect, "Index " & $ni & " is out of bound " & $a.len)
  c.rmove a.link_size * ni

proc skip(c: Continent) =
  let t = DataKind c.read_byte
  case t
  of dkNatural: c.rmove c.read_byte
  of dkString: c.rmove c.read_natural
  of dkArray:
    c.move 1
    let a = c.read
    c.go_link a.link_size
    c.skip
  of dkStaticLink: c.rmove c.read_byte
  of dkDynamicLink:
    raise new_exception(ValueError, "Dynamic links not supported yet")

proc array*(c: Continent) =
  c.stack.add c.pos

proc `end`*(c: Continent) =
  let begin = c.stack.pop
  let max_pos = c.pos

  var cur_write = c.pos
  var cur_read = cur_write
  var length = 0

  while true:
    if cur_read == begin:
      break

    c.pos = cur_write
    c.write_bytes to_seq cur_read
    cur_write = c.pos

    c.pos = cur_read
    c.skip
    cur_read = c.pos
    length += 1

  c.pos = cur_write
  c.write(length, write_type = false)
  let link_size = to_seq len to_seq max_pos
  if link_size.len > 1:
    raise new_exception(ValueError, "Link size " & $link_size & " is too big for one byte")
  c.write_bytes link_size
  c.write dkArray


proc save(p: Path) =
  p.pos = p.c.pos

proc load(p: Path) =
  p.c.pos = p.pos

proc read*(p: Path): Data =
  p.load
  p.c.read

proc `[]`*(p: Path, i: int64): Path =
  new(result)
  let a = p.c.read
  if a.kind != dkArray:
    raise new_exception(ValueError, "Indexing only supported for data of dkArray kind")
  result.c = p.c
  result.parent = some p
  result.c.move_to_element(a, i)
  result.c.go_link a.link_size
  result.save

proc `[]`*(c: Continent, i: int64): Path =
  new(result)
  c.rpos = 0
  result.c = c
  result = result[i]

proc write*(c: Continent, cl: StaticLink) =
  do_assert c == cl.c
  c.write(cl.pos, write_type = false)
  c.write_bytes @[uint8 dkStaticLink]

proc test() =
  proc test_plain(payload: seq[Data]) =
    let c = "../test.bin".new_continent
    for p in payload:
      c.write p
    c.rpos = 0
    for i in countdown(payload.len - 1, 0):
      check payload[i] == c.read

  test_plain @[new_data 1234, new_data "abcd"]

  proc test_array(payload: seq[Data]) =
    let c = "../test.bin".new_continent
    c.array
    for p in payload:
      c.write p
    c.`end`
    c.rpos = 0
    for i, p in payload:
      check c[i].read == p

  test_array @[new_data 1234, new_data "abcd"]

  block test_sub_array:
    let c = "../test.bin".new_continent
    with c:
      array
      write 1234
      array
      write 1234
      write "abcd"
      `end`
      write "abcd"
      `end`
    check c[0].read == new_data 1234
    check c[1][0].read == new_data 1234
    check c[1][1].read == new_data "abcd"
    check c[2].read == new_data "abcd"

if is_main_module:
  test()
