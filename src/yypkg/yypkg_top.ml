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

open Types
open Yylib

(* all the options we accept *)
let cmd_line_spec =
  let mk ~n ~h c = Args.spec ~name:n ~help:h ~children:c in
  [
    mk ~n:"--prefix" ~h:"prefix yypkg will be working in" [];
    mk ~n:"--init" ~h:"setups a directory tree for yypkg (run once)" [];
    mk ~n:"--install" ~h:"install a package (extension is .txz)" [];
    Web_install.cli_spec;
    mk ~n:"--upgrade" ~h: "upgrade with package (extension is .txz)" [
      mk ~n:"--install-new" ~h:"If package isn't already installed, install it." [];
    ];
    mk ~n:"--uninstall" ~h:"uninstall a package by name" [];
    mk ~n:"--list" ~h:"list the packages installed" [];
    Config.cli_spec;
    Repository.cli_spec;
    Makepkg.cli_spec;
]

(* upgrade with or without -install-new *)
let upgrade old_cwd cmd_line = 
  let f ?install_new l =
    let l = Args.to_string_list l in
    let l = List.rev_map (FilePath.DefaultPath.make_absolute old_cwd) l in
    Db.update (Upgrade.upgrade ?install_new (Conf.read ()) l)
  in
  match List.partition Args.is_opt cmd_line with
  | [ Args.Opt ("--install-new", l) ], [] -> f ~install_new:true l
  | [], l -> f l
  | _ -> assert false

let main b =
  if Args.wants_help () || Args.nothing_given () then
    Args.bprint_spec b 0 (Args.usage_msg cmd_line_spec "yypkg")
  else
    let cmd_line = Args.parse cmd_line_spec Sys.argv in
    (* the second cmd_line is the first with occurences of "-prefix" removed *)
    let prefix, cmd_line = Yylib.prefix_of_cmd_line cmd_line in
    let action, actionopts = Yylib.action_of_cmd_line cmd_line in
    if action = Some "--init" && actionopts = [] then
      (* setups a few things for correct operation of yypkg, see yypkg/init.ml*)
      let prefix = FilePath.DefaultPath.make_absolute (Sys.getcwd ()) prefix in
      Init.init prefix
    else
      (* Keep track of the original working dir. *)
      let start_dir = Sys.getcwd () in
      (* Some operations are relative to the prefix so chdir to it. *)
      let old_cwd = Sys.getcwd () in
      Sys.chdir prefix;
      Yylib.sanity_checks ();
      match action with
      | None -> ()
      (* install, accepts several packages at once *)
      | Some "--install" ->
          let l = List.rev_map (FilePath.DefaultPath.make_absolute old_cwd)
            (Args.to_string_list actionopts) in
          Db.update (Install.install (Conf.read ()) l)
      (* web-install, accepts several packages at once *)
      | Some "--web-install" -> Web_install.main ~start_dir actionopts
      (* upgrade, accepts several packages at once *)
      | Some "--upgrade" -> upgrade old_cwd actionopts
      (* uninstall, accepts several packages at once *)
      | Some "--uninstall" ->
          let l = Args.to_string_list actionopts in
          Db.update (Uninstall.uninstall l)
      (* list the installed packages *)
      | Some "--list" ->
          let l = Args.to_string_list actionopts in
          Yylist.list (Db.read ()) l
      (* config does nothing on its own but has suboptions *)
      | Some "--config" -> Config.main actionopts
      (* *)
      | Some "makepkg" -> Makepkg.main actionopts
      (* *)
      | Some "--repository" -> Repository.main actionopts
      (* if an option was different, Args.parse would already have
       * complained, so this final pattern will never be matched *)
      | _ -> assert false

let main_wrap b =
  try main b with 
  | Args.Incomplete_parsing (opts, sl) as e ->
      Args.bprint_spec b 0 (Args.usage_msg cmd_line_spec "yypkg");
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
