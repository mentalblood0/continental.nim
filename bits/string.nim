import std/bitops
import std/unittest
import std/sugar
import std/sequtils
import std/unicode
import std/tables

import common
import natural

type EncodingDict* = tuple[rune_to_code: Table[Rune, uint8],
    code_to_rune: Table[uint8, Rune]]

func new_encoding_dict*(runes: seq[Rune]): EncodingDict =
  for r in runes:
    let code = uint8 len result.rune_to_code
    result.rune_to_code[r] = code
    result.code_to_rune[code] = r

func contains(d: EncodingDict, r: Rune): bool = r in d.rune_to_code

func len(d: EncodingDict): uint8 = uint8 len d.rune_to_code

func code_size(d: EncodingDict): uint8 = uint8 fast_log2 len d

func code(d: EncodingDict, r: Rune): uint8 = d.rune_to_code[r]
func rune(d: EncodingDict, code: uint8): Rune = d.code_to_rune[code]

func new_bits*(runes: seq[Rune], d: EncodingDict): Bits =
  new(result)
  for r in runes:
    if r in d:
      let c = d.code r
      for i in 0 ..< d.code_size.int:
        if c.test_bit i:
          result.add_1
        else:
          result.add_0

func new_runes*(bits: Bits, d: EncodingDict): seq[Rune] =
  for batch in bits.batches int code_size d:
    let code = block:
      var r: uint8
      for i, b in batch:
        if b:
          r.set_bit i
      r
    result.add d.rune code

proc test*() =
  let d = new_encoding_dict @[
    "ф", "ы", "в", "а", "п", "р", "о", "л", "д", "ж", "я", "ч",
    "с", "м",
    "и", "т", "ь", "б", "ю", "э", "ъ", "й", "ц", "у", "к", "е",
    "н", "г",
    "ш", "щ", "з", "х",
  ].map c => c.to_runes[0]
  let s = "абвгд".to_runes
  let bits = s.new_bits d
  check 25 == len bits
  check s == bits.new_runes d

if is_main_module:
  test()
