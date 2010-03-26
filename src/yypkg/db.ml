open Types
open Yylib

let install_package db script = 
  script :: db

let uninstall_package db name = 
  List.filter (fun s -> not (package_is_named name s)) db

let read () =
  try db_of_sexp (Disk.read db_path) with _ -> []

let write db =
  (* We sort the db because, err, no reason, it won't even be more readable
   * considering the size of the database but in the case one has to edit the db
   * by hand, it's always nicer to have it sorted.
   * We want to use stable_sort to keep the (perfectly fine) old behaviour when
   * there are multiple packages with the same name.
   * We DO NOT WANT to sort the content of each package as it may (and actually
   * will) have unexpected consequences upon package removal *)
  let sorted_db = List.stable_sort compare db in
  Disk.write db_path (sexp_of_db sorted_db)

(* read the database, run the function, write the database to disk
 * if fail raises an exception, nothing will be written :-) *)
let update (f : db -> db) =
  write (f (read ()))

