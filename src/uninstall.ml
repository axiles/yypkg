open Types
open Lib

let file_can_be_removed file other_packages =
  (* not (List.exists (file_exists_in_package file) other_packages) *)
  if List.exists (file_exists_in_package file) other_packages then
    let () = Printf.printf "Not removed: file %s exists in another package" file
    in
    false
  else
    true

let execute_uninstall_action (script, install_results) other_pkgs = function
    | RM p -> assert false
        (* id, "" *)
    | Reverse id -> 
        let f (action_id, results) =
          action_id = id
        in
        let rm s =
          if file_can_be_removed s other_pkgs then rm s else ()
        in
        let results = List.find_all f install_results in
        let g (_, results ) =
          match results with
            | Filelist l -> List.iter rm l
            | NA -> ()
        in
        List.iter g results

let uninstall_package db package_name =
  let pkg= List.find (fun ((m, _, _), _) -> m.package_name = package_name) db in
  let (metadata, _, uninstall_actions) as script, _ = pkg in
  let other_pkgs = List.filter (fun ((m, _, _), _) -> m.package_name <> package_name) db in
  let _= List.map (execute_uninstall_action pkg other_pkgs) uninstall_actions in
  let updated_db = Db.uninstall_package db script in
  updated_db

