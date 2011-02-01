open Types
open Lib
open Yylib

let uninstall_and_install yypkg_conf l db =
  let uninstalled = Uninstall.uninstall l db in
  Install.install yypkg_conf l uninstalled

(* Upgrade by installing the new package and then uninstalling the old one. *)
let upgrade yypkg_conf db p =
  assert_file_exists p;
  let metadata = metadata_of_script (open_package p) in
  let pred_holds = Config.predicate_holds yypkg_conf.preds in
  match List.partition pred_holds metadata.predicates with
  | _, [] -> 
      assert (is_installed db p);
      uninstall_and_install yypkg_conf [ p ] db
  |_, f_preds -> raise (Unmatched_predicates f_preds)

let upgrade yypkg_conf l db =
  List.fold_left (upgrade yypkg_conf) db l
