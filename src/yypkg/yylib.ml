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

exception Package_does_not_exist
exception File_not_found of string

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

let rev_list_of_queue q =
  Queue.fold (fun l e -> e::l) [] q

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
  if x.[0] = 'x' && x.[1] = ' ' then
    String.sub x 2 (String.length x - 2)
  else
    x

let reduce_path path =
  FilePath.DefaultPath.reduce path

(* run the command cmd and return a list of lines of the output *)
let command cmd =
  (* XXX: pid! *)
  (* FIXME: read_ic 0 (Unix.open_process_in cmd) *)
  []

(* mkdir for use in installation scripts: it returns the path that got created
 * so it can be registered and reversed upon uninstallation *)
let mkdir path_unexpanded =
  let path = expand_environment_variables path_unexpanded in
  let () = FileUtil.mkdir ~parent:true ~mode:0o755 path in
  [ path_unexpanded ]

(* tar xf the folder 'i' in the package 'pkg' to the folder 'p' *)
let expand pkg i p =
  (* XXX: package_script.el should always use "/" separators, otherwise we have
   * a problem between platforms: maybe add an entry to set the separator *)
  let l = (List.length (Lib.split_path ~dir_sep:"/" i)) - 1 in
  let pkg = expand_environment_variables pkg in
  let iq = expand_environment_variables i in
  let pq = expand_environment_variables p in
  if not (Sys.file_exists p) then ignore (mkdir p) else ();
  let x = Lib.from_tar (`extract (pq, string_of_int l, iq)) pkg in
  (* bsdtar already strips the beginning of the path *)
  let xx = List.rev_map filter_bsdtar_output x in
  List.rev_map (Lib.strip_component ~prefix:p ~dir_sep:"/" 0) xx

(* rm with verbose output
 *   doesn't fail if a file doesn't exist
 *   removes directories only if empty *)
let rm path_unexpanded =
  (* Sys.file_exists and most others will follow symlinks
   * This means that if 'y' points to 'x' but 'x' doesn't exist, Sys.file_exists
   * will return false even though 'y' exists *)
  let exists path =
    try let () = ignore (Unix.lstat path) in true with _ -> false
  in
  (* FIXME: env var souldn't be kept in the database, they have to be expanded
   * before *)
  let path = expand_environment_variables path_unexpanded in
  if exists path then
    (* Sys.file_exists follows symlink and is therefore not usable *)
    if FileUtil.test FileUtil.Is_dir path then
      if [| |] = Sys.readdir path then
        let () = FileUtil.rm ~recurse:true [ path ] in
        Printf.printf "Removed: %s\n" path
      else
        Printf.printf "Not removed (directory not empty): %s\n" path
    else
      let () = FileUtil.rm [ path ] in
      Printf.printf "Removed: %s\n" path
  else
    Printf.printf "Not removed (doesn't exist): %s\n" path

(* checks if a file exists in any package in a given database *)
let file_exists_in_package file (_, result_list) =
  let f = function
    | _, NA -> false
    | _, Filelist l -> List.exists ((=) file) l
  in
  List.exists f result_list

(* Return the yypkg metadata from a sherpa package *)
let metadata_of_pkg ((m, _, _), _) =
  m

(* a predicate to check a package has some name, used with List.find *)
let package_is_named name p =
  (metadata_of_pkg p).name = name

(* Find a package by its name *)
let find_by_name db name =
  List.find (package_is_named name) db

(* check a file exists: raises an exception with the name of the missing file if
  * it doesn't *)
let assert_file_exists f =
  if not (Sys.file_exists f) then
    raise (File_not_found f)

(* various sanity checks:
  * do etc/yypkg.conf and /var/log/packages/yypkg_db exist?
  * TODO: check the external binaries are available 
  * ... *)
let sanity_checks () =
  let required_files = [ db_path; conf_path; sherpa_conf_path ] in
  List.iter assert_file_exists required_files
