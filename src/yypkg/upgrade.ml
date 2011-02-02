open Types
open Lib
open Yylib

(* Upgrade by installing the new package and then uninstalling the old one. *)
let upgrade ?(install_new=false) yypkg_conf db p =
  assert_file_exists p;
  let metadata = metadata_of_script (open_package p) in
  let pred_holds = Config.predicate_holds yypkg_conf.preds in
  match List.partition pred_holds metadata.predicates with
  | _, [] -> 
      if is_installed db metadata.name then
        let uninstalled = Uninstall.uninstall [ metadata.name ] db in
        Install.install yypkg_conf [ p ] uninstalled
      else
        if install_new then
          Install.install yypkg_conf [ p ] db
        else
          raise (Not_upgrading_not_installed_package p)
  |_, f_preds -> raise (Unmatched_predicates f_preds)

let upgrade ?install_new yypkg_conf l db =
  List.fold_left (upgrade ?install_new yypkg_conf) db l
