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
  "-install", [], "install a package (ends in .t{xz,gz,bz2}";
  "-uninstall", [], "uninstall a package by name";
  "-list", [], "list the packages installed";
  "-config", [
    "-setpreds", [], "set a predicate: \"arch=x86_64-w64-mingw32\"";
    "-delpreds", [], "remove a predicate";
    "-listpreds", [], "list predicates";
    (* not handled currently: "-regen", []; *)
    "-set-tar-kind", [], "choose between \"gnu\" and \"bsd\" tar";
  ], "parent option for:";
  "-init", [], "setups a directory tree for yypkg (run once)";
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
    | _ -> raise (Args.Parsing_failed "PREFIX environment variable not found and no -prefix specified")

(* find the action from a command-line, only one allowed at a time *)
let action_of_cmd_line cmd_line = 
  (* we want all options (Args.Opt _) and discard all values *)
  let lt, _ = List.partition Args.is_opt cmd_line in
  match lt with
    (* exactly one action: everything ok *)
    | [ Args.Opt (action, subopts) ] -> action, subopts
    (* zero or several actions is forbidden: raise an exception that will be
     * caught later on *)
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
              let vals = List.map Args.val_of_opts preds in
              List.fold_left Config.setpred conf vals
          | Args.Opt ("-delpreds", preds) -> 
              let vals = List.map Args.val_of_opts preds in
              List.fold_left Config.delpred conf vals
          | Args.Opt ("-set-tar-kind", kinds) -> begin
              match kinds with
                | [ kind ] -> 
                    let kind = Args.val_of_opts kind in
                    Config.set_tar_kind conf kind
                | _ -> raise (Args.Parsing_failed "Only one tar_kind can be given at once.")
            end
          (* Args makes sure this last case can't happen *)
          | _ -> assert false
        in
        Conf.update (fun conf -> List.fold_left f conf opts)
      end
    (* if -listpred has been given together with another argument: *)
    | _ -> raise (Args.Parsing_failed "⁻listpred can't be combined with other arguments.")

let main () =
  if Args.wants_help () then
    Args.print_help cmd_line_spec
  else
    let cmd_line = Args.parse cmd_line_spec Sys.argv in
    (* the second cmd_line is the first with occurences of "-prefix" removed *)
    let prefix, cmd_line = prefix_of_cmd_line cmd_line in
    let action, actionopts = action_of_cmd_line cmd_line in
    if ("-init", []) = (action, actionopts) then
      (* setups a few things for correct operation of yypkg, see yypkg/init.ml*)
      Init.init prefix
    else
      (* Some operations are relative to the prefix so chdir to it *)
      let old_cwd = Sys.getcwd () in
      Sys.chdir prefix;
      Yylib.sanity_checks ();
      match action, actionopts with
        (* install, accepts one package at a time *)
        | "-install", [Args.Val s] ->
            let s = FilePath.DefaultPath.make_absolute old_cwd s in
            Db.update (Install.install s (Conf.read ()))
        (* uninstall, accepts one package at a time *)
        | "-uninstall", [ Args.Val s ] -> Db.update (Uninstall.uninstall s)
        (* list the installed packages *)
        | "-list", [] -> 
            List.iter (fun p -> print_endline (name_of_package p)) (Db.read ())
        (* config does nothing on its own but has suboptions *)
        | "-config", subopts -> config subopts
        (* if an option was different, Args.parse would already have complained,
         * so this final pattern will never be matched *)
        | _ -> assert false

let () =
  (* FIXME: if an exception happens, no matter what it is, the error message
   * will always be the same: the command-line is wrong, even if it was
   * perfectly fine (a.g. -list but the database being corrupt *)
  try main () with 
    | Args.Incomplete_parsing (opts, sl) ->
        Args.print_spec 0 (Args.usage_msg cmd_line_spec)
    | Args.Parsing_failed s as e -> raise e
    | Yylib.File_not_found p when Yylib.db_path = p || Yylib.conf_path = p ->
        prerr_endline "You forgot to run -init or something got corrupted."
    | Yylib.File_not_found p as e -> raise e
    | Unmatched_predicates l ->
        let f (b, v) = Printf.eprintf "Predicate %s = %s doesn't hold.\n" b v in
        List.iter f l

