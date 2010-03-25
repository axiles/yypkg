open Types
open Yylib

let install_package db script = 
  script :: db

let uninstall_package db name = 
  List.filter (fun s -> not (package_is_named name s)) db

let read db_path =
  db_of_sexp (Disk.read db_path)

let write db_path db =
  Disk.write db_path (sexp_of_db db)
