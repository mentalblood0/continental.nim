import std/sets
import std/math
import std/syncio

import common

type
  Continent* = ref object
    path*: string
    file: File
    offset: Natural
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

proc new_continent*(path: string): Continent =
  new(result)
  result.path = path
  result.file = open(path, fm_read_write)

proc pos(c: Continent): Natural =
  c.file.get_file_pos * 8 + c.offset

proc `pos=`(c: Continent, value: Natural) =
  c.file.set_file_pos value div 8
  c.offset = value mod 8

proc reset(c: Continent) =
  c.file.set_file_pos 0, fsp_end
  c.offset = 0

proc move(c: Continent, value: Natural) =
  c.pos = c.pos + value

proc rmove(c: Continent, value: Natural) =
  c.pos = c.pos - value

proc read_bits(c: Continent, n: Natural): Bits =
  new(result)
  c.rmove n
  let bits_to_read = n + c.offset.int
  let bytes_to_read = bits_to_read div 8
  let new_offset = bits_to_read mod 8
  result = block:
    var r: Bits
    r.buffer.set_len(bytes_to_read)
    do_assert c.file.read_bytes(r.buffer, 0, r.buffer.len) == r.buffer.len
    r[c.offset, bits_to_read - 1]
  c.rmove n

proc write_bits(c: Continent, b: Bits) =
  do_assert c.file.write_bytes(b, 0, b.len) == b.len

proc test*() =
  let c = "../../test.bin".new_continent
  let bits = new_bits 1234
  c.write_bits bits
  c.reset
  check c.read_bits == bits

if is_main_module:
  test()
