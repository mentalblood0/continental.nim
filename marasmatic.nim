import std/syncio
import std/parseopt
import std/options
import std/unicode
import std/sugar
import std/sequtils

let f = block:
  var path: string
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      path = key
    else:
      continue
  open path, fm_read

const alphabet = @["ф", "ы", "в", "а", "п", "р", "о", "л", "д", "ж",
    "я", "ч", "с", "м", "и", "т", "ь", "б", "ю", "э", "ъ", "й",
    "ц", "у", "к", "е", "н", "г", "ш", "щ", "з", "х",
    "ё"].map c => c.to_runes[0]
const words_separators = @[" ", ",", ":"].map c => c.to_runes[0]
const sentences_separators = @[".", "!", "?", ";"].map c => c.to_runes[0]

iterator runes(f: File): Rune =
  var buffer: seq[char]
  buffer.set_len 2

  while true:
    if 0 == f.read_chars buffer:
      break
    yield buffer.to_runes[0]

type
  WordKind = enum
    wkNormal
    wkWordsSep
    wkSentSep

  Word = tuple[kind: WordKind, content: seq[Rune]]

iterator words(f: File): Word =
  var buf: seq[Rune]
  for r in f.runes:
    if r in alphabet:
      buf.add r
      continue
    if buf.len > 0:
      yield (wkNormal, buf)
    if r in words_separators:
      yield (wkWordsSep, @[r])
    elif r in sentences_separators:
      yield (wkSentSep, @[r])
    buf.set_len 0

for w in f.words:
  echo w
