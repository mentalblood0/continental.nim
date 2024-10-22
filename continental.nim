import std/syncio
import std/sets
import std/options
import std/with
import std/unittest

type IntegerEncodingError = object of RangeDefect

func to_seq(i: Natural): seq[uint8] =
  var c = i
  while true:
    result.add(uint8 c mod 256)
    c = c div 256
    if c == 0:
      break

func size(i: Natural): Natural =
  if i == 0:
    return 1
  var j = i
  while j > 0:
    result += 1
    j = j div 256

func to_seq_fixed(i: Natural, size: Natural): seq[uint8] =
  block input_validation:
    let minimum_size = i.size
    if i.size > size:
      raise new_exception(IntegerEncodingError, "Can not encode natural " & $i &
          " in " & $size & " bytes (minimum " & $minimum_size & " bytes requried")
  let s = to_seq i
  for k in 1 .. size - s.len:
    result.add(uint8 0)
  result &= s

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
    dkDynamicLink

  Path* = ref object
    c: Continent
    p: OrderedSet[Natural]
    last: Natural

  DynamicLink = tuple[path: seq[int]]

  DataObj = object
    case kind*: DataKind
    of dkNatural: nat*: Natural
    of dkString: str*: string
    of dkArray:
      len*: Natural
      link_size: Natural
    of dkDynamicLink: dl*: DynamicLink

  Data = ref DataObj

func new_link(path: seq[int]): DynamicLink =
  do_assert path.len > 0
  (path: path)

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

proc reset(c: Continent) =
  c.file.set_file_pos 0, fsp_end

proc move(c: Continent, value: Natural) =
  c.file.set_file_pos value, fsp_cur

proc rmove(c: Continent, value: Natural) =
  c.file.set_file_pos -value, fsp_cur

proc read_bytes(c: Continent, n: Natural): seq[uint8] =
  c.rmove n
  result = block:
    var r: seq[uint8]
    r.set_len(n)
    do_assert c.file.read_bytes(r, 0, r.len) == r.len
    r
  c.rmove n

proc read_chars(c: Continent, n: Natural): string =
  result.set_len n
  let bytes = c.read_bytes n
  for i, b in bytes:
    result[i] = char b

proc read_byte(c: Continent): uint8 =
  c.read_bytes(1)[0]

proc write_bytes(c: Continent, b: seq[uint8]) =
  do_assert c.file.write_bytes(b, 0, b.len) == b.len

proc write(c: Continent, t: DataKind) =
  c.write_bytes(@[uint8 t])

proc write*(c: Continent, i: Natural) =
  c.write_bytes i.to_seq & i.size.to_seq & @[uint8 dkNatural]

proc read_natural(c: Continent): Natural =
  to_natural c.read_bytes Natural c.read_byte

proc write*(c: Continent, s: string) =
  let b = block:
    var r: seq[uint8]
    r.set_len s.len
    for i, c in s:
      r[i] = uint8 c
    r
  c.write_bytes b
  c.write_bytes to_seq s.len
  c.write_bytes s.len.size.to_seq_fixed 1
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

proc `[]`*(c: Continent, i: int): Path
proc `[]`*(p: Path, i: int): Path
proc read(c: Continent): Data =
  var path: OrderedSet[Natural]
  while true:
    if path.contains_or_incl c.pos:
      raise new_exception(ValueError, "Recursion detected, positions stack is " & $path)
    let t = DataKind c.read_byte
    case t
    of dkNatural:
      return Data(kind: dkNatural, nat: c.read_natural)
    of dkString:
      return Data(kind: dkString, str: c.read_chars c.read_natural)
    of dkArray:
      let ls = Natural c.read_byte
      let l = c.read_natural
      return Data(kind: dkArray, len: l, link_size: ls)
    of dkDynamicLink:
      let segment_size = to_natural c.read_bytes 1
      let length = to_natural c.read_bytes to_natural c.read_bytes 1
      let dl = block:
        var p: seq[int]
        p.set_len length
        for i in 0 ..< length:
          p[i] = to_natural c.read_bytes segment_size
        new_link p
      c.pos = block:
        var path = c[dl.path[0]]
        for i in 1 ..< dl.path.len:
          path = path[dl.path[i]]
        path.last

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
  of dkDynamicLink:
    let segment_size = to_natural c.read_bytes 1
    let length = to_natural c.read_bytes to_natural c.read_bytes 1
    c.rmove segment_size * length

proc array*(c: Continent) =
  c.stack.add c.pos

proc `end`*(c: Continent) =
  let begin = c.stack.pop
  let link_size = size c.pos

  var cur_write = c.pos
  var cur_read = cur_write
  var length = 0

  while true:
    if cur_read == begin:
      break

    c.pos = cur_write
    c.write_bytes cur_read.to_seq_fixed link_size
    cur_write = c.pos

    c.pos = cur_read
    c.skip
    cur_read = c.pos
    length += 1

  c.pos = cur_write
  c.write_bytes to_seq length
  c.write_bytes length.size.to_seq_fixed 1
  c.write_bytes link_size.to_seq_fixed 1
  c.write dkArray

proc read*(p: Path): Data =
  p.c.pos = p.last
  p.c.read

proc add(p: var Path) =
  let a = p.c.pos
  p.p.incl p.last
  do_assert a notin p.p
  p.last = a

proc `[]`*(p: Path, i: int): Path =
  let a = p.c.read
  if a.kind != dkArray:
    raise new_exception(ValueError, "Indexing only supported for data of dkArray kind")
  result = p
  result.c.move_to_element a, i
  result.c.go_link a.link_size
  result.add

proc `[]`*(c: Continent, i: int): Path =
  new(result)
  reset c
  result.c = c
  result = result[i]

proc write*(c: Continent, dl: DynamicLink) =
  let segment_size = size max dl.path
  for n in dl.path:
    c.write_bytes n.to_seq_fixed segment_size
  c.write_bytes dl.path.len.to_seq
  c.write_bytes dl.path.len.size.to_seq_fixed 1
  c.write_bytes segment_size.to_seq_fixed 1
  c.write_bytes @[uint8 dkDynamicLink]

proc test() =
  proc test_plain(payload: seq[Data]) =
    let c = "../test.bin".new_continent
    for p in payload:
      c.write p
    reset c
    for i in countdown(payload.len - 1, 0):
      check payload[i] == c.read

  test_plain @[new_data 1234, new_data "abcd"]

  proc test_array(payload: seq[Data]) =
    let c = "../test.bin".new_continent
    c.array
    for p in payload:
      c.write p
    c.`end`
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

  block test_dynamic_link:
    let c = "../test.bin".new_continent
    with c:
      array
      array
      write 0
      write new_link @[1]
      `end`
      array
      write 1
      write new_link @[0]
      `end`
      `end`
    check c[0][1][0].read == new_data 1
    check c[1][1][0].read == new_data 0

  block test_recursive_link:
    let c = "../test.bin".new_continent
    with c:
      array
      write new_link @[0]
      `end`
    discard c[0]
    expect ValueError:
      discard c[0].read
    expect ValueError:
      discard c[0][0]

  block test_recursive_links:
    let c = "../test.bin".new_continent
    with c:
      array
      write new_link @[1]
      write new_link @[0]
      `end`
    for i in 0 .. 1:
      discard c[i]
      expect ValueError:
        discard c[i].read

if is_main_module:
  test()
