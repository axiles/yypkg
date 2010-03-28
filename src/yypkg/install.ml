open Types
open Yylib

let execute_install_action package (id, action) =
  match action with
    | AHK p -> id, Filelist (command ((String.concat " " (ahk_bin :: p)))) (* quote *)
    | Expand (i, p) -> id, Filelist (expand package i p)
    | Exec p -> id, Filelist (command (String.concat " " p)) (* quote *)
    | MKdir p -> id, Filelist (mkdir p)

let install_package db package =
  let (metadata, install_actions, _ as script) = open_package package in
  let () = print_endline "package opened" in
  let results = List.map (execute_install_action package) install_actions in
  let updated_db = Db.install_package db (script, List.rev results) in
  updated_db

let install p db =
  let p = FilePath.DefaultPath.make_absolute Lib.install_path p in
  if Sys.file_exists p then
    install_package db p
  else
    raise File_not_found

