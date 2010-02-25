open Types
open Yylib

let execute_install_action package (id, action) =
  match action with
    | AHK p -> id, Filelist (command ((String.concat " " (ahk_bin :: p)))) (* quote *)
    | Expand (i, p) -> id, Filelist (expand package i p)
    | Shortcut (p, l) -> assert false
        (* id, Filelist "" (* FIXME *) *)
    | Exec p -> id, Filelist (command (String.concat " " p)) (* quote *)
    | MKdir p -> id, Filelist (mkdir p)

let install_package db package =
  let (metadata, install_actions, _ as script) = open_package package in
  let () = print_endline "package opened" in
  let results = List.map (execute_install_action package) install_actions in
  let updated_db = Db.install_package db (script, List.rev results) in
  updated_db

