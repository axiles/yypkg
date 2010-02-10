open Types
open Lib

let install db p () =
  let p = FilePath.DefaultPath.make_absolute install_path p in
  assert (Sys.file_exists p);
  let updated_db = Install.install_package db p in
  Db.write db_path updated_db

let uninstall db p () =
  assert (List.exists (fun ((m, _, _), _) -> m.package_name = p) db);
  let updated_db = Uninstall.uninstall_package db p in
  Db.write db_path updated_db

let list db _ () =
  List.iter (function (m, _, _), _ -> print_endline m.package_name) db

let main () =
  let db = Db.read db_path in
  let f = ref (fun () -> ()) in
  let prefix = ref "" in
  let set_f g s =
    f := g db s
  in
  let lst = [
    "-prefix", Arg.Set_string prefix, "prefix for the package management (mandatory)";
    "-install", Arg.String (set_f install), "install a package";
    "-uninstall", Arg.String (set_f uninstall), "uninstall a package";
    "-list", Arg.String (set_f list), "list packages";
  ]
  in
  let usage_msg = "pouet" in
  let () = Arg.parse lst (fun _ -> ()) usage_msg in
  assert ("" <> !prefix );
  let () = ignore (mkdir !prefix) in
  let () = Sys.chdir !prefix in
  !f ()

let () = main ()
