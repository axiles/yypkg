(*
 * yypkg - A cross-platforma package manager
 * Copyright (C) 2010 Adrien Nader
 * Copyright (C) <year>  <name of author>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Types
open Yylib

exception Bad_prefix_specification of Args.opt list

(* all the options we accept *)
let cmd_line_spec = [
  "-prefix", [], "prefix yypkg will be working in";
  "-install", [], "install a package (extension is .txz)";
  "-upgrade", [
    "-install-new", [], "If package isn't already installed, install it.";
  ], "upgrade with package (extension is .txz)";
  "-uninstall", [], "uninstall a package by name";
  "-list", [], "list the packages installed";
  "-config", [
    "-setpreds", [], "set a predicate: \"arch=x86_64-w64-mingw32\"";
    "-delpreds", [], "remove a predicate";
    "-listpreds", [], "list predicates";
    (* not handled currently: "-regen", []; *)
  ], "parent option for:";
  "-init", [], "setups a directory tree for yypkg (run once)";
]

(* find the prefix from a command-line *)
let prefix_of_cmd_line cmd_line =
  let lt, lf = List.partition (Args.is_opt ~s:"-prefix") cmd_line in
  match lt with
  (* we've not been given -prefix, maybe ${YYPREFIX} ? *)
  | [] when (try ignore (Sys.getenv "YYPREFIX");true with Not_found -> false) ->
      Sys.getenv "YYPREFIX", lf
  (* we've been given the -prefix with a string argument
   * we also set it as an env var so it can be used in install scripts *)
  | [ Args.Opt (_, [ Args.Val prefix ]) ] -> 
      Unix.putenv "YYPREFIX" prefix;
      prefix, lf
  (* all other combinations are invalid: raise an exception that will be
   * caught later on *)
  | _ -> raise (Args.Parsing_failed "YYPREFIX environment variable not found and -prefix specified")

(* find the action from a command-line, only one allowed at a time *)
let action_of_cmd_line cmd_line = 
  (* we want all options (Args.Opt _) and discard all values *)
  let lt, _ = List.partition Args.is_opt cmd_line in
  match lt with
    (* exactly one action: everything ok *)
    | [ Args.Opt (action, subopts) ] -> Some action, subopts
    (* no action: whatever the default will be *)
    | [] -> None, []
    (* several actions is forbidden: raise an exception that will be caught
     * later on *)
    | _ -> raise (Args.Parsing_failed "Only one action is allowed at once.")

let config opts =
  match List.partition (Args.is_opt ~s:"-listpreds") opts with
  (* first: if -listpreds has been given, print the configuration predicates
   * -listpreds must not be given together with other arguments *)
  | _, [] -> Conf.print_preds (Conf.read ())
  (* We're *very* nice to the user here: it's possible to add or remove
   * several predicates at once, and to add, remove some, add again predicates
   * in a single call to yypkg *)
  | [], opts -> begin
      let f conf = function
        | Args.Opt ("-setpreds", preds) ->  
            let preds = Args.to_string_list preds in
            List.fold_left Config.setpred conf preds
        | Args.Opt ("-delpreds", preds) -> 
            let preds = Args.to_string_list preds in
            List.fold_left Config.delpred conf preds
        (* Args makes sure this last case can't happen *)
        | _ -> assert false
      in
      Conf.update (fun conf -> List.fold_left f conf opts)
    end
  (* if -listpred has been given together with another argument: *)
  | _ -> raise (Args.Parsing_failed "⁻listpred can't be combined with other arguments.")

(* upgrade with or without -install-new *)
let upgrade old_cwd cmd_line = 
  let f ?install_new l =
    let l = Args.to_string_list l in
    let l = List.rev_map (FilePath.DefaultPath.make_absolute old_cwd) l in
    Db.update (Upgrade.upgrade ?install_new (Conf.read ()) l)
  in
  match List.partition Args.is_opt cmd_line with
  | [ Args.Opt ("-install-new", l) ], [] -> f ~install_new:true l
  | [], l -> f l
  | _ -> assert false

let main b =
  if Args.wants_help () || Args.nothing_given () then
     Args.bprint_help b cmd_line_spec
  else
    let cmd_line = Args.parse cmd_line_spec Sys.argv in
    (* the second cmd_line is the first with occurences of "-prefix" removed *)
    let prefix, cmd_line = prefix_of_cmd_line cmd_line in
    let action, actionopts = action_of_cmd_line cmd_line in
    if action = Some "-init" && actionopts = [] then
      (* setups a few things for correct operation of yypkg, see yypkg/init.ml*)
      Init.init prefix
    else
      (* Some operations are relative to the prefix so chdir to it *)
      let old_cwd = Sys.getcwd () in
      Sys.chdir prefix;
      Yylib.sanity_checks ();
      match action with
      | None -> ()
      | Some action ->
          match action, actionopts with
          (* install, accepts several packages at once *)
          | "-install", l ->
              let l = List.rev_map (FilePath.DefaultPath.make_absolute old_cwd)
                (Args.to_string_list l) in
              Db.update (Install.install (Conf.read ()) l)
          (* upgrade, accepts several packages at once *)
          | "-upgrade", l -> upgrade old_cwd l
          (* uninstall, accepts several packages at once *)
          | "-uninstall", l ->
              let l = Args.to_string_list l in
              Db.update (Uninstall.uninstall l)
          (* list the installed packages *)
          | "-list", l ->
              let l = Args.to_string_list l in
              Yylist.list (Db.read ()) l
          (* config does nothing on its own but has suboptions *)
          | "-config", subopts -> config subopts
          (* if an option was different, Args.parse would already have
           * complained, so this final pattern will never be matched *)
          | _ -> assert false

let main_wrap b =
  (* FIXME: if an exception happens, no matter what it is, the error message
   * will always be the same: the command-line is wrong, even if it was
   * perfectly fine (e.g. -list but the database being corrupt *)
  try main b with 
  | Args.Incomplete_parsing (opts, sl) as e ->
      Args.bprint_spec b 0 (Args.usage_msg cmd_line_spec);
      raise e
  | Args.Parsing_failed s as e -> raise e
  | File_not_found p as e when Yylib.db_path = p || Yylib.conf_path = p ->
      Buffer.add_string b "You forgot to run -init or something got corrupted.";
      raise e
  | File_not_found p as e -> raise e
  | Unmatched_predicates l as e ->
      let f (p, v) = Printf.bprintf b "Predicate %s = %s doesn't hold.\n" p v in
      List.iter f l;
      raise e

let main_wrap_wrap b =
  try main_wrap b with e ->
    Buffer.add_string b "\nFailure. An exception has been raised:\n";
    Buffer.add_string b (Printexc.to_string e);
    Buffer.add_string b "\n\nThe backtrace is as folllows:\n";
    Buffer.add_string b (Printexc.get_backtrace ())