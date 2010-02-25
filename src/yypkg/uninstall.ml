open Types
open Yylib

let file_can_be_removed file other_packages =
  (* not (List.exists (file_exists_in_package file) other_packages) *)
  if List.exists (file_exists_in_package file) other_packages then
    let () = Printf.printf "Not removed (exists in another package): %s\n" file
    in
    false
  else
    true

let execute_uninstall_action (_, install_results) other_pkgs = function
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
  let pkgs, other_pkgs = List.partition (package_is_named package_name) db in
  let f (((_, _, u_acts), m) as pkg) =
    List.iter (execute_uninstall_action pkg other_pkgs) u_acts
  in
  let () = List.iter f pkgs in
  Db.uninstall_package db package_name
