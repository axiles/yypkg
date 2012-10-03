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

open Printf
open Types

let ahk_bin =
  Lib.filename_concat [ Lib.install_path; "ahk.exe" ]

let conf_folder =
  Lib.filename_concat [ "etc"; "yypkg.d" ]

let db_folder =
  Lib.filename_concat [ "var"; "log"; "packages" ]

let db_path =
  Lib.filename_concat [ db_folder; "yypkg_db" ]

let default_download_path =
  Lib.filename_concat [ "var"; "cache"; "packages" ]

let conf_path =
  Lib.filename_concat [ "etc"; "yypkg.d"; "yypkg.conf" ]

let sherpa_conf_path =
  Lib.filename_concat [ "etc"; "yypkg.d"; "sherpa.conf" ]

(* replace env var of the form ${windir} *)
let expand_environment_variables s =
  let env_var_re = Str.regexp "\\${\\([0-9A-Za-z_]+\\)}" in
  if Str.string_match env_var_re s 0 then
    let repl = Unix.getenv (Str.matched_group 1 s) in
    Str.replace_first env_var_re repl s
  else
    s

(* bsdtar writes 'x some/path/foo' during extraction *)
let filter_bsdtar_output x =
  if String.length x >= 7 && x.[0] = 'b' && x.[1] = 's' && x.[2] = 'd'
    && x.[3] = 't' && x.[4] = 'a' && x.[5] = 'r' && x.[6] = ':' then
      (prerr_endline x;
      raise Lib.Skip)
  else
    if String.length x >= 2 && x.[0] = 'x' && x.[1] = ' ' then
      String.sub x 2 (String.length x - 2)
    else
      x

(* run the command cmd and return a list of lines of the output *)
let command cmd =
  (* TODO: quote the commands? *)
  Lib.split_by_line (Lib.run_and_read (Array.of_list cmd) `stdout)

(* mkdir for use in installation scripts: it returns the path that got created
 * so it can be registered and reversed upon uninstallation *)
let mkdir path_unexpanded =
  let path = expand_environment_variables path_unexpanded in
  FileUtil.mkdir ~parent:true ~mode:0o755 path;
  [ path_unexpanded ]

(* tar xf the folder 'in_' in the package 'pkg' to the folder 'p' *)
let expand pkg in_ p =
  (* NOTE: package_script.el should always use "/" separators, otherwise we have
   * a problem between platforms: maybe add an entry to set the separator *)
  let l = (List.length (Lib.split_path ~dir_sep:"/" in_)) - 1 in
  let pkg = expand_environment_variables pkg in
  let iq = expand_environment_variables in_ in
  let pq = expand_environment_variables p in
  if not (Sys.file_exists p) then ignore (mkdir p) else ();
  let x = Lib.from_tar (`extract (pq, string_of_int l, iq)) pkg in
  (* bsdtar already strips the beginning of the path *)
  let xx = Lib.list_rev_map_skip filter_bsdtar_output x in
  List.rev_map (Lib.strip_component ~prefix:p ~dir_sep:"/" 0) xx

(* rm with verbose output
 *   doesn't fail if a file doesn't exist
 *   removes directories only if empty *)
let rm path_unexpanded =
  (* Sys.file_exists and most others will follow symlinks
   * This means that if 'y' points to 'x' but 'x' doesn't exist, Sys.file_exists
   * will return false even though 'y' exists *)
  let exists path =
    try ignore (Unix.lstat path); true with _ -> false
  in
  (* FIXME: env var souldn't be kept in the database, they have to be expanded
   * before *)
  let path_expanded = expand_environment_variables path_unexpanded in
  let path = FilePath.DefaultPath.reduce path_expanded in
  if exists path then
    (* Sys.file_exists follows symlink and is therefore not usable *)
    if FileUtil.test FileUtil.Is_dir path then
      if [| |] = Sys.readdir path then
        let () = FileUtil.rm ~recurse:true [ path ] in
        Printf.eprintf "Removed: %s\n" path
      else
        Printf.eprintf "Not removed (directory not empty): %s\n" path
    else
      let () = FileUtil.rm [ path ] in
      Printf.eprintf "Removed: %s\n" path
  else
    Printf.eprintf "Not removed (doesn't exist): %s\n" path

(* checks if a file exists in any package in a given database *)
let file_exists_in_package file (_, result_list) =
  let f = function
    | _, Filelist l -> List.mem file l
    | _ -> false
  in
  List.exists f result_list

(* Return the yypkg metadata from a sherpa package *)
let metadata_of_pkg ((m, _, _), _) =
  m

let metadata_of_script (m, _, _) =
  m

(* a predicate to check a package has some name, used with List.find *)
let package_is_named name p =
  (metadata_of_pkg p).name = name

let package_name_matches_regexp regexp p =
  Str.string_match regexp (metadata_of_pkg p).name 0

(* Find a package by its name *)
let find_by_name db name =
  List.find (package_is_named name) db

(* Find if a package by name, matching a regular exression *)
let find_all_by_name_regex db re =
  List.find_all (package_name_matches_regexp re) db

(* Test if a package is installed, by name *)
let is_installed db p =
  List.exists (package_is_named p) db

(* Test if a package is installed, by name, matching a regular exression *)
let is_installed_regex db re =
  List.exists (package_name_matches_regexp re) db

(* various sanity checks:
  * do etc/yypkg.conf and /var/log/packages/yypkg_db exist?
  * TODO: check the external binaries are available 
  * ... *)
let sanity_checks () =
  let required_files = [ db_path; conf_path; sherpa_conf_path ] in
  List.iter Lib.assert_file_exists required_files

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
  | _ -> raise (Args.Parsing_failed "YYPREFIX environment variable not found and -prefix not specified")

