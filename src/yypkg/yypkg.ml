open Types
open Yylib

exception Bad_prefix_specification of Args.opt list

(* all the options we accept *)
let cmd_line_spec = [
  "-prefix", [];
  "-install", [];
  "-uninstall", [];
  "-list", [];
  (* not handled currently *)
  "-config", [
    "-setpreds", [];
    "-delpreds", [];
    "-listpreds", [];
    "-regen", [];
  ]
]

(* find the prefix from a command-line *)
let prefix_of_cmd_line cmd_line =
  let lt, lf = List.partition (Args.is_opt ~s:"-prefix") cmd_line in
  match lt with
    (* we've not been given -prefix, if the YYPREFIX env var is missing, this
     * will raise Not_found: we'll catch it and display the usage message *)
    | [] -> Sys.getenv "YYPREFIX", lf
    (* we've been given the -prefix with a string argument, no problem here *)
    | [ Args.Opt (_, [ Args.Val prefix ]) ] -> prefix, lf
    (* all other combinations are invalid: raise an exception that will be
     * caught later on *)
    | _ -> assert false

(* find the action from a command-line, only one allowed at a time *)
let action_of_cmd_line cmd_line = 
  (* we want all options (Args.Opt _) and discard all values *)
  let lt, _ = List.partition Args.is_opt cmd_line in
  match lt with
    (* exactly one action: everything ok *)
    | [ Args.Opt (action, subopts) ] -> action, subopts
    (* zero or several actions is forbidden: raise an exception that will be
     * caught later on *)
    | _ -> assert false

let config opts =
  match List.partition (Args.is_opt ~s:"-list") opts with
    | _, [] ->
        List.iter (fun (b, v) -> Printf.printf "%s = %s\n" b v) (Conf.read ())
    | [], opts -> begin
        let f conf = function
          | Args.Opt ("-setpreds", preds) ->  
              let vals = List.map Args.val_of_opts preds in
              List.fold_left Config.setpred conf vals
          | Args.Opt ("-delpreds", preds) -> 
              let vals = List.map Args.val_of_opts preds in
              List.fold_left Config.delpred conf vals
          | _ -> assert false
        in
        Conf.update (fun conf -> List.fold_left f conf opts)
      end
    | _ -> assert false

let main () =
  let cmd_line = Args.parse cmd_line_spec Sys.argv in
  let prefix, cmd_line = prefix_of_cmd_line cmd_line in
  let action, actionopts = action_of_cmd_line cmd_line in
  let () = ignore (mkdir prefix) in
  let () = Sys.chdir prefix in
  match action, actionopts with
    | "-install", [ Args.Val s ] -> Db.update (Install.install s)
    | "-uninstall", [ Args.Val s ] -> Db.update (Uninstall.uninstall s)
    | "-list", [] -> 
        List.iter (fun p -> print_endline (name_of_package p)) (Db.read ())
    | "-config", subopts -> config subopts
    | _, _ -> assert false

let () =
  let usage_msg = "bad usage, will make better error message tomorrow" in
  try main () with e -> prerr_endline usage_msg; raise e

