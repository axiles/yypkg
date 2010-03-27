open Types
open Yylib

exception Package_does_not_exist
exception File_not_found
exception Bad_prefix_specification of Args.opt list


let install p db =
  let p = FilePath.DefaultPath.make_absolute Lib.install_path p in
  if Sys.file_exists p then
    let updated_db = Install.install_package db p in
    Db.write db_path updated_db
  else
    raise File_not_found

let uninstall p db =
  if List.exists (package_is_named p) db then
    let updated_db = Uninstall.uninstall_package db p in
    Db.write db_path updated_db
  else
    raise Package_does_not_exist

let list _ db =
  let compare ((ma, _, _), _) ((mb, _, _), _) =
    compare ma mb
  in
  let l = List.sort compare db in
  List.iter (function (m, _, _), _ -> Printf.printf "%s\n" m.package_name) l

let parse_cmd_line cmd_line =
  let prefix_of_cmd_line cmd_line =
    let lt, lf = List.partition (Args.find_opt_pred "-prefix") cmd_line in
    match lt with
      (* we've not been given -prefix, if the YYPREFIX env var is missing, this
       * will raise Not_found: we'll catch it and display the usage message *)
      | [] -> Sys.getenv "YYPREFIX", lf
      (* we've been given the -prefix with a string argument, no problem here *)
      | [ Args.Opt (_, [ Args.Val prefix ]) ] -> prefix, lf
      (* all other combinations are invalid: raise an exception that will be
       * caught later on *)
      | _ -> assert false
  in
  let action_of_cmd_line cmd_line = 
    (* we want all options (Args.Opt _) and discard all values *)
    let lt, _ = List.partition Args.find_opts_pred cmd_line in
    match lt with
      (* exactly one action: everything ok *)
      | [ Args.Opt (action, subopts) ] -> action, subopts
      (* zero or several actions is forbidden: raise an exception that will be
       * caught later on *)
      | _ -> assert false
  in
  (* all the options we accept *)
  let cmd_line_spec = [
    "-prefix", [];
    "-install", [];
    "-uninstall", [];
    "-list", [];
    (* not handled currently *)
    "-config", [
      "-preds", [];
      "-regen", [];
    ]
  ]
  in
  let cmd_line = Args.parse cmd_line_spec Sys.argv in
  let prefix, cmd_line = prefix_of_cmd_line cmd_line in
  let action, actionopts = action_of_cmd_line cmd_line in
  let f = match action, actionopts with
    | "-install", [ Args.Val s ] -> install s
    | "-uninstall", [ Args.Val s ] -> uninstall s
    | "-list", [] -> list ()
    | "-config", [] -> fun _ -> ()
    | _, _ -> assert false
  in
  f, prefix

let main () =
  let usage_msg = "bad usage, will make better error message tomorrow" in
  let f, prefix =
    try parse_cmd_line Sys.argv with e -> prerr_endline usage_msg; raise e
  in
  let () = ignore (mkdir prefix) in
  let () = Sys.chdir prefix in
  let db = Db.read db_path in
  f db

let () = main ()
