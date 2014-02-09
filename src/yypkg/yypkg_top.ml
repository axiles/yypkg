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
    mk ~n:"--web-install" ~h:"download and install a package by name" [
      mk ~n:"--follow-dependencies" ~h:"also fetch and install dependencies" [];
      mk ~n:"--download-only" ~h:"download packages" [];
      mk ~n:"--download-folder" 
        ~h:("where to put downloaded files (instead of " ^ Yylib.default_download_path ^ ")") [];
    ];
    mk ~n:"--upgrade" ~h: "upgrade with package (extension is .txz)" [
      mk ~n:"--install-new" ~h:"If package isn't already installed, install it." [];
    ];
    mk ~n:"--uninstall" ~h:"uninstall a package by name" [];
    mk ~n:"--list" ~h:"list the packages installed" [];
    mk ~n:"--config" ~h:"parent option for:" [
      mk ~n:"--predicates" ~h:"" [
        mk ~n:"--set" ~h:"set predicates (e.g. \"host=x86_64-w64-mingw32\")" [];
        mk ~n:"--delete" ~h:"delete predicates" [];
        mk ~n:"--list" ~h:"list predicates" [];
      ];
      mk ~n:"--set-mirror" ~h:"set the mirror to use" [];
      (* add --print *)
    ];
    mk ~n:"--repository" ~h:"" [
      mk ~n:"--generate" ~h:"generate repository data" [];
    ];
    mk ~n:"--makepkg" ~h:"" [
      mk ~n:"--output" ~h:"output directory (defaults to current dir)" [];
      mk ~n:"--script" ~h:"package script file (- for stdin)" [];
      mk ~n:"--iscripts" ~h:"directory of install scripts" [];
      mk ~n:"--directory" ~h:"directory to package" [];
      mk ~n:"--template" ~h:"write a template script on stdout" [];
    ];
]

type repository_opts = {
  generate : string;
}

type web_install_opts = {
  follow_dependencies : bool;
  download_only : bool;
  dest : string;
  packages : string list;
}

let makepkg opts =
  let init =
    { Makepkg.output = ""; script = ""; install_scripts = None;
      directory = Makepkg.dir_of_path ""; template = false } in
  let l = [
    "--output", (fun ~accu n o ->
      { accu with Makepkg.output = Args.(get string n o) });
    "--script", (fun ~accu n o ->
      { accu with Makepkg.script = Args.(get string n o) });
    "--iscripts", (fun ~accu n o ->
      { accu with Makepkg.install_scripts = Some (Makepkg.dir_of_path Args.(get string n o)) });
    "--directory", (fun ~accu n o ->
      { accu with Makepkg.directory = Makepkg.dir_of_path Args.(get string n o) });
    "--template", (fun ~accu n o ->
      { accu with Makepkg.template = Args.(get bool n o) });
  ]
  in
  let opts = Args.foo ~where:"--makepkg" ~init l opts in
  Makepkg.main opts

let repository opts =
  let l = [
    "--generate", (fun ~accu n o ->
      { accu with generate = Args.(get string n o) })
  ]
  in
  let init = { generate = "" } in
  let opts = Args.foo ~where:"--repository" ~init l opts in
  Repository.generate opts.generate

let web_install ~start_dir opts =
  let init = { follow_dependencies = false; download_only = false;
    packages = []; dest = Yylib.default_download_path } in
  let l = [
    "--download-only", (fun ~accu n o ->
      { accu with download_only = Args.(get bool n o) });
    "--follow-dependencies", (fun ~accu n o ->
      { accu with follow_dependencies = Args.(get bool n o) });
    "--download-folder", (fun ~accu n o ->
      { accu with dest = FilePath.make_absolute start_dir Args.(get string n o) });
    "--packages", (fun ~accu n o ->
      { accu with packages = Args.(get string n o) :: accu.packages });
  ] in
  let o = Args.foo ~init ~where:"--web-install" l opts in
  let conf = Conf.read () in
  let packages = get_packages ~conf ~follow:o.follow_dependencies ~dest:o.dest ~packages:o.packages in
  (if not o.download_only then Db.update (Install.install conf packages))

type predicates = {
  list : bool;
  set : string list;
  delete : string list;
}

let predicates opts =
  let init = { list = false; set = []; delete = [] } in
  let l = [
    "--list", (fun ~accu n o ->
      { accu with list = Args.(get bool n o) });
    "--set", (fun ~accu n o ->
      { accu with set = Args.(get string n o) :: accu.set });
    "--delete", (fun ~accu n o ->
      { accu with delete = Args.(get string n o) :: accu.delete });
  ] in
  let o = Args.foo ~init ~where:"--predicates" l opts in
  match o.list, o.set, o.delete with
  | true, [], [] ->
      Conf.print_predicates (Conf.read ())
  | true, _, _ ->
      Lib.ep "WARNING: both --list and --(un)set given to --config; only --list is executed.\n%!";
      Conf.print_predicates (Conf.read ())
  | false, [], [] ->
      Lib.ep "WARNING: none of --list, --set, --unset given to --config.\n%!";
  | _, _, [] ->
      Conf.update (fun conf -> List.fold_left Config.setpred conf o.set)
  | _, [], _ ->
      Conf.update (fun conf -> List.fold_left Config.delpred conf o.delete)
  | _, _, _ ->
      Lib.ep "WARNING: both --set and --unset given to --config; doing nothing.\n%!"

let config opts =
  ListLabels.iter opts ~f:(function
  | Args.Opt ("--predicates", subopts) ->
      predicates opts
  | Args.Opt ("--set-mirror", [ Args.Val mirror ]) ->
      Conf.update (fun conf -> Config.set_mirror conf mirror)
  | Args.Opt ("--set-mirror", _) ->
     raise (Args.Parsing_failed "--set-mirror requires a string as argument.")
  | Args.Opt (name, _) ->
     raise (Args.Parsing_failed (Lib.sp "Unknown argument `%s'." name))
  | Args.Val v ->
     raise (Args.Parsing_failed (Lib.sp "--config requires a sub-option, not `%s'." v))
  )

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
      | Some "--web-install" -> web_install ~start_dir actionopts
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
      | Some "--config" -> config actionopts
      (* *)
      | Some "--repository" -> repository actionopts
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
