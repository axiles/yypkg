open Types
open Lib

let install_path = "install_path"

let parse_args argv =
  match argv.(1) with
    | "-add"
    | "-install" -> Install argv.(2)
    | "-remove"
    | "-uninstall" -> Uninstall argv.(2)
    | "-list" -> List
    | _ -> assert false

let main () =
  let db = Db.read db_path in
  match parse_args Sys.argv with
    | Install p -> 
        assert (Sys.file_exists p);
        let updated_db = Install.install_package db p in
        Db.write db_path updated_db
    | Uninstall p -> 
        assert (List.exists (fun ((m, _, _), _) -> m.package_name = p) db);
        let updated_db = Uninstall.uninstall_package db p in
        Db.write db_path updated_db
    | List ->
        List.iter (function (m, _, _), _ -> print_endline m.package_name) db

let () = main ()
