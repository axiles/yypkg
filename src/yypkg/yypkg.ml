open Types
open Yylib

exception Bad_prefix_specification of Args.opt list

(* all the options we accept *)
let cmd_line_spec = [
  "-prefix", [];
  "-install", [];
  "-uninstall", [];
  "-list", [];
  "-config", [
    "-setpreds", [];
    "-delpreds", [];
    "-listpreds", [];
    (* not handled currently: "-regen", []; *)
  ];
  "-init", [];
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
  match List.partition (Args.is_opt ~s:"-listpreds") opts with
    (* first: if -listpreds has been given, print the configuration predicates
     * -listpreds must not be given together with other arguments *)
    | _, [] ->
        List.iter (fun (b, v) -> Printf.printf "%s = %s\n" b (String.concat ","
        v)) (Conf.read ())
    (* We're *very* nice to the user here: it's possible to add or remove
     * several predicates at once, and to add, remove some, add again predicates
     * in a single call to yypkg *)
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
    (* if -listpred has been given together with another argument: *)
    | _ -> assert false

let main () =
  let cmd_line = Args.parse cmd_line_spec Sys.argv in
  (* the second cmd_line is the first with occurences of "-prefix" removed *)
  let prefix, cmd_line = prefix_of_cmd_line cmd_line in
  let action, actionopts = action_of_cmd_line cmd_line in
  (* We just got the prefix, let's chdir to it since some operations will be
   * relative to it *)
  let () = ignore (mkdir prefix) in
  let () = Sys.chdir prefix in
  match action, actionopts with
    (* install, accepts one package at a time *)
    | "-install", [ Args.Val s ] -> Db.update (Install.install s (Conf.read ()))
    (* uninstall, accepts one package at a time *)
    | "-uninstall", [ Args.Val s ] -> Db.update (Uninstall.uninstall s)
    (* list the installed packages *)
    | "-list", [] -> 
        List.iter (fun p -> print_endline (name_of_package p)) (Db.read ())
    (* setups a few things for correct operation of yypkg, see yypkg/init.ml *)
    | "-init", [] -> Init.init ()
    (* config does nothing on its own but has suboptions which are handled in
     * another function *)
    | "-config", subopts -> config subopts
    (* if an option were different, Args.parse would already have complained, so
     * this final pattern will never be matched *)
    | _ -> assert false

let () =
  (* FIXME: fix the usage message... *)
  let usage_msg = "bad usage, will make better error message tomorrow" in
  (* FIXME: if an exception happens, no matter what it is, the error message
   * will always be the same: the command-line is wrong, even if it was
   * perfectly fine (a.g. -list but the database being corrupt *)
  try main () with e -> prerr_endline usage_msg; raise e

