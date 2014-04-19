(*
 * yypkg - A cross-platform package manager
 * Copyright (C) 2010-2014 Adrien Nader
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

(** This module exists only in order to run the main by default only in
 * command-line mode (as opposed to GUI). *)

open Types

(* all the options we accept *)
let cmd_line_spec =
  let mk ~n ~h c = Args.spec ~name:n ~help:h ~children:c in
  [
    mk ~n:"--prefix" ~h:"prefix yypkg will be working in" [];
    mk ~n:"--init" ~h:"setups a directory tree for yypkg (run once)" [];
    mk ~n:"--install" ~h:"install a package (extension is .txz)" [];
    Web.cli_spec;
    mk ~n:"--upgrade" ~h: "upgrade with package (extension is .txz)" [
      mk ~n:"--install-new" ~h:"If package isn't already installed, install it." [];
    ];
    mk ~n:"--uninstall" ~h:"uninstall a package by name" [];
    mk ~n:"--list" ~h:"list the packages installed" [];
    Config.cli_spec;
    Repository.cli_spec;
    Makepkg.cli_spec;
    Deploy.cli_spec;
]

(* find the prefix from a command-line *)
let prefix_of_cmd_line cmd_line =
  match List.partition (Args.is_opt ~s:"--prefix") cmd_line with
  (* we've not been given --prefix, maybe ${YYPREFIX} ? *)
  | [], lf ->
      (try Some (Sys.getenv "YYPREFIX") with Not_found -> None), lf
  (* we've been given the --prefix with a string argument
   * we also set it as an env var so it can be used in install scripts *)
  | [ Args.Opt (_, [ Args.Val prefix ]) ], lf ->
      Unix.putenv "YYPREFIX" prefix;
      Some prefix, lf
  (* all other combinations are invalid *)
  | _, lf -> None, lf

(* find the action from a command-line, only one allowed at a time *)
let action_of_cmd_line cmd_line =
  (* we want all options (Args.Opt _) and discard all values *)
  match List.filter Args.is_opt cmd_line with
  (* exactly one action: everything ok *)
  | [ Args.Opt (action, subopts) ] -> Some action, subopts
  (* no action: whatever the default will be *)
  | [] -> None, []
  (* several actions is forbidden: raise an exception that will be caught
   * later on *)
  | _ -> raise (Args.Parsing_failed "Exactly one action is allowed at once.")

(* upgrade with or without -install-new *)
let upgrade old_cwd cmd_line =
  let f ?install_new l =
    let l = Args.to_string_list l in
    let l = List.rev_map (FilePath.DefaultPath.make_absolute old_cwd) l in
    Db.update (Upgrade.upgrade ?install_new (Config.read ()) l)
  in
  match List.partition Args.is_opt cmd_line with
  | [ Args.Opt ("--install-new", l) ], [] -> f ~install_new:true l
  | [], l -> f l
  | _ -> assert false

let prefix_not_set () =
  raise (Args.Parsing_failed
    "YYPREFIX environment variable not found and --prefix not specified")

(* Some operations are relative to the prefix and yypkg will chdir to it. *)
let enter = function
  | Some prefix -> Yylib.sanity_checks prefix; Sys.chdir prefix
  | None -> prefix_not_set ()

let installed prefix =
  try
    Yylib.sanity_checks prefix;
    true
  with e ->
    false

let main b =
  let cmd_line = Args.parse cmd_line_spec Sys.argv in
  (* the second cmd_line is the first with occurences of "-prefix" removed *)
  let prefix, cmd_line = prefix_of_cmd_line cmd_line in
  let action, actionopts = action_of_cmd_line cmd_line in
  (* Keep track of the original working dir. *)
  let old_cwd = Sys.getcwd () in
  match action with
  (* init, setups a few things for correct operation of yypkg *)
  | Some "--init" ->
      (match prefix with
      | Some prefix ->
          Init.init (FilePath.DefaultPath.make_absolute old_cwd prefix)
      | None -> prefix_not_set ())
  (* install, accepts several packages at once *)
  | Some "--install" ->
      enter prefix;
      let l = List.rev_map (FilePath.DefaultPath.make_absolute old_cwd)
        (Args.to_string_list actionopts) in
      Db.update (Install.install (Config.read ()) l)
  (* web-install, accepts several packages at once *)
  | Some "--web-install" ->
      enter prefix;
      Web.main ~start_dir:old_cwd actionopts
  (* upgrade, accepts several packages at once *)
  | Some "--upgrade" ->
      enter prefix;
      upgrade old_cwd actionopts
  (* uninstall, accepts several packages at once *)
  | Some "--uninstall" ->
      enter prefix;
      Db.update (Uninstall.uninstall (Args.to_string_list actionopts))
  (* list the installed packages *)
  | Some "--list" ->
      enter prefix;
      Yylist.list (Db.read ()) (Args.to_string_list actionopts)
  (* config, makepkg and repository do nothing on their own but have
   * suboptions *)
  | Some "--config" ->
      enter prefix;
      Config.main actionopts
  | Some "--makepkg" ->
      Makepkg.main actionopts
  | Some "--repository" ->
      Repository.main actionopts
  | Some "--deploy" ->
      Deploy.main actionopts
  (* if an option was different, Args.parse would already have complained,
   * so this final pattern will never be matched *)
  | _ -> assert false

let main_wrap b =
  let fail e =
    Buffer.add_string b "\nFailure. An exception has been raised:\n";
    Buffer.add_string b (Printexc.to_string e);
    Buffer.add_string b "\n\nThe backtrace is as folllows:\n";
    Buffer.add_string b (Printexc.get_backtrace ())
  in
  try main b with
  | Args.Incomplete_parsing (opts, sl) as e ->
      Args.bprint_spec b 0 (Args.usage_msg cmd_line_spec "yypkg");
      fail e
  | Args.Parsing_failed s as e -> fail e
  | File_not_found p as e when Yylib.db_path = p || Yylib.conf_path = p ->
      Buffer.add_string b "You forgot to run -init or something got corrupted.";
      fail e
  | File_not_found p as e -> fail e
  | Unmatched_predicates l as e ->
      let f (p, v) = Printf.bprintf b "Predicate %s = %s doesn't hold.\n" p v in
      List.iter f l;
      fail e
  | e -> fail e

let () =
  Printexc.record_backtrace true;
  if Args.nothing_given () && Lib.started_from_windows_gui () then
    if installed Lib.install_path then (
      enter (Some Lib.install_path);
      try
        VBUI.main ()
      with e ->
        ignore VBUI.(msgbox ~title:"Fatal error" ~buttons:[ Button.critical ] (
          String.concat "\n" [
            Printexc.to_string e;
            Printexc.get_backtrace ();
          ]
        ))
    )
    else
      Deploy.main Args.([ Opt ("--host", [ Val "Native Windows" ]) ])
  else
    let b = Buffer.create 1000 in
    (if Args.nothing_given () || Args.wants_help () then
      Args.bprint_spec b 0 (Args.usage_msg cmd_line_spec "yypkg")
    else
      main_wrap b);
    Buffer.output_buffer stderr b;
    if Buffer.length b <> 0 then
      exit 1

