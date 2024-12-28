# Regular expression support is provided by the PCRE library package,
# which is open source software, written by Philip Hazel, and copyright
# by the University of Cambridge, England. Source can be found at
# ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/

import std/[json, sugar, strutils, nre, cmdline, os, logging, strformat]
import db_connector/db_sqlite

proc create_tables(db: DbConn) =
  db.exec(
    sql"""create table if not exists messages(
    chat_id int not null,
    message_id int not null,
    unique(chat_id, message_id))"""
  )
  db.exec(sql"create table if not exists words(value text unique not null)")
  db.exec(sql"create unique index if not exists words_value on words(value)")
  db.exec(
    sql"""create table if not exists transitions(
    current_word int not null,
    next_word int not null,
    foreign key(current_word) references words(rowid),
    foreign key(next_word) references words(rowid))"""
  )
  db.exec(
    sql"create index if not exists transitions_current_word on transitions(current_word)"
  )
  db.exec(
    sql"""create table if not exists transitions_messages(
    transition int not null,
    message int not null,
    foreign key(transition) references transition(rowid),
    foreign key(message) references messages(rowid),
    unique(transition, message))"""
  )
  db.exec(
    sql"create index if not exists transitions_messages_transition on transitions_messages(transition)"
  )

proc load(db: DbConn, j: JsonNode, user_id: string) =
  db.exec(sql"insert or ignore into words (value) values (?)", ".")
  let chat_id = j["id"].get_int
  for m in j["messages"].elems:
    if m["type"].get_str != "message":
      continue
    if m["from_id"].get_str != user_id:
      continue
    let m_id = m["id"].get_int
    let m_rowid = db.get_value(
      sql"insert into messages (chat_id, message_id) values (?, ?) returning rowid",
      chat_id,
      m_id,
    )
    let mt = block:
      let mt_seq = block:
        if m["text"].kind == JString:
          @[m["text"].get_str]
        else:
          collect:
            for t in m["text"].elems:
              if t.kind == JString:
                t.get_str
              else:
                t["text"].get_str
      mt_seq.join
    var splitted = mt.find_all(re"(*UTF8)[A-Za-zА-Яа-я]+|\.|!|\?|;|,|-")
    if splitted.len == 0:
      continue
    if splitted[^1] notin [".", "?", "!"]:
      splitted.add "."
    var prev_w = "."
    for w in splitted:
      db.exec(sql"insert or ignore into words (value) values (?)", w)
      let t_rowid = db.get_value(
        sql"""insert into transitions (current_word, next_word)
        select c.rowid, n.rowid from words as c join words as n 
        on c.value == ? and n.value == ? returning rowid""",
        prev_w,
        w,
      )
      db.exec(
        sql"insert or ignore into transitions_messages (transition, message) values (?, ?)",
        t_rowid,
        m_rowid,
      )
      prev_w = w

proc write(db_path: string, user_id: string, jsons_paths: seq[string]) =
  let db = open(db_path, "", "", "")
  db.exec(sql"pragma synchronous=off")
  db.exec(sql"pragma locking_mode=exclusive")
  db.exec(sql"pragma journal_mode=memory")
  db.create_tables
  for p in jsons_paths:
    debug &"{db_path} <-- {p}"
    db.load(p.parse_file, user_id)
  db.close()

when is_main_module:
  let log_handler = new_console_logger(fmt_str = "$date $time $levelname ")
  add_handler log_handler

  let dbs_dir = get_data_dir() / "continental"
  create_dir dbs_dir

  let user_id = param_str 2
  do_assert ".." notin user_id

  let db_path = dbs_dir / user_id & ".db"

  case param_str 1
  of "create":
    do_assert not db_path.file_exists
    db_path.write(user_id, command_line_params()[3 .. ^1])
  of "replace":
    db_path.remove_file
    db_path.write(user_id, command_line_params()[3 .. ^1])
  of "generate":
    do_assert db_path.file_exists
    let db = open(db_path, "", "", "")
    db.exec(sql"pragma query_only=true")
    let amount = parse_int param_str 3
    var prev_w = "."
    for i in 1 .. amount:
      prev_w = db.get_value(
        sql"""select wn.value from words as wc join transitions as t join words as wn
        on wc.value == ? and t.current_word == wc.rowid and wn.rowid == t.next_word order by random() limit 1""",
        prev_w,
      )
      if prev_w.len == 0:
        prev_w = "."
        continue
      if i != 1 and prev_w notin [".", "!", "?", ",", ";"]:
        stdout.write " "
      stdout.write prev_w
    stdout.write "\n"
    db.close()
