open Types
open Lib

exception Package_does_not_exist
exception File_not_found

let install p db =
  let p = FilePath.DefaultPath.make_absolute install_path p in
  if Sys.file_exists p then
    let updated_db = Install.install_package db p in
    Db.write db_path updated_db
  else
    raise File_not_found

let uninstall p db =
  if List.exists (fun ((m, _, _), _) -> m.package_name = p) db then
    let updated_db = Uninstall.uninstall_package db p in
    Db.write db_path updated_db
  else
    raise Package_does_not_exist

let list _ db =
  List.iter (function (m, _, _), _ -> print_endline m.package_name) db

let main () =
  let f = ref (fun _ -> ()) in
  let prefix = ref "" in
  let set_f g s =
    f := g s
  in
  let lst = [
    "-prefix", Arg.Set_string prefix, "prefix for the package management (mandatory)";
    "-install", Arg.String (set_f install), "install a package";
    "-uninstall", Arg.String (set_f uninstall), "uninstall a package";
    "-list", Arg.Unit (fun () -> set_f list "dummy"), "list packages";
  ]
  in
  let usage_msg = "pouet" in
  let () = Arg.parse lst (fun _ -> ()) usage_msg in
  if "" = !prefix then
    invalid_arg "Missing mandatory argument: -prefix"
  else
    let () = ignore (mkdir !prefix) in
    let () = Sys.chdir !prefix in
    let db = Db.read db_path in
    !f db 

let () = main ()
