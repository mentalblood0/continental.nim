import std/syncio
import std/tables
import std/parseopt
import std/options
import std/unicode
import std/sugar
import std/sequtils
import std/times

import continental

let execution_start = cpu_time()

let args = block:
  var input_path: string
  var output_path: string
  for kind, key, val in getopt():
    case kind
    of cmdLongOption, cmdShortOption:
      case key
      of "i", "input":
        input_path = val
      of "o", "output":
        output_path = val
    else:
      continue
  (i: open(input_path, fm_read), o: new_continent(output_path))

const alphabet = @[
  "ф", "ы", "в", "а", "п", "р", "о", "л", "д", "ж", "я", "ч", "с", "м",
  "и", "т", "ь", "б", "ю", "э", "ъ", "й", "ц", "у", "к", "е", "н", "г",
  "ш", "щ", "з", "х", "ё",
].map c => c.to_runes[0]
const words_separators = @[" ", ",", ":"].map c => c.to_runes[0]
const sentences_separators = @[".", "!", "?", ";"].map c => c.to_runes[0]

iterator runes(f: File): Rune =
  var buf: string
  while f.read_line buf:
    for r in buf.to_runes:
      yield r.to_lower

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

var dict: OrderedTable[seq[Rune], Natural]
var next: seq[Table[Natural, Natural]]
block read_file:
  var prev: Option[seq[Rune]]
  for w in args.i.words:
    case w.kind
    of wkNormal:
      if w.content notin dict:
        next.add Table[Natural, Natural]()
        dict[w.content] = dict.len
      if is_some prev:
        if dict[get prev] >= len next:
          next.add Table[Natural, Natural]()
        if dict[w.content] notin next[dict[get prev]]:
          next[dict[get prev]][dict[w.content]] = 0
        next[dict[get prev]][dict[w.content]] += 1
      prev = some w.content
    of wkWordsSep:
      discard
    of wkSentSep:
      prev = none seq[Rune]

args.o.array
args.o.array
for k in dict.keys:
  args.o.write $k
args.o.`end`
args.o.array
for prev in next:
  if prev.len == 0:
    continue
  args.o.array
  for n, freq in prev.pairs:
    args.o.array
    args.o.write n
    args.o.write freq
    args.o.`end`
  args.o.`end`
args.o.`end`
args.o.`end`

let execution_end = cpu_time()
echo execution_end - execution_start
