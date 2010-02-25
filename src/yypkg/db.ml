open Sexplib
open Types
open Yylib

let install_package db script = 
  script::db
  
let uninstall_package db name =
  List.filter (fun s -> not (package_is_named name s)) db

let read db_path =
  try 
    let ic = open_in db_path in
    let db = db_of_sexp (Sexp.input_sexp ic) in
    let () = close_in ic in
    db
  with
    _ -> []

let write db_path db =
  let sexp_db = sexp_of_db db in
  let oc = open_out_gen [Open_creat; Open_binary; Open_wronly; Open_trunc] 0o644 db_path in
  let () = Sexp.output_hum oc sexp_db in
  close_out oc

