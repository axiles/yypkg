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

exception Unknown_package of string

external remove : string -> unit = "yy_remove"
external create_reparse_point : string -> string -> unit = "create_reparse_point"

let conf_dir =
  Lib.filename_concat [ "etc"; "yypkg.d" ]

let db_dir =
  Lib.filename_concat [ "var"; "log"; "packages" ]

let db_path =
  Lib.filename_concat [ db_dir; "yypkg_db" ]

let default_download_path =
  Lib.filename_concat [ "var"; "cache"; "packages" ]

let conf_path =
  Lib.filename_concat [ conf_dir; "yypkg.conf" ]

(* replace env var of the form ${windir} *)
let expand_environment_variables s =
  let env_var_re = Str.regexp "\\${\\([0-9A-Za-z_]+\\)}" in
  if Str.string_match env_var_re s 0 then
    let repl = Unix.getenv (Str.matched_group 1 s) in
    Str.replace_first env_var_re repl s
  else
    s

(* run the command cmd and return a list of lines of the output *)
let command cmd =
  (* TODO: quote the commands? *)
  Lib.split_by_line (Lib.run_and_read (Array.of_list cmd) `stdout)

(* mkdir for use in installation scripts: it returns the path that got created
 * so it can be registered and reversed upon uninstallation *)
let mkdir path_unexpanded =
  let path = expand_environment_variables path_unexpanded in
  FileUtil.mkdir ~parent:true ~mode:0o755 path;
  path

(* tar xf the directory 'in_' from the package 'pkg' to the directory 'p' *)
let expand archive in_ p =
  (* NOTE: package_script.el should always use "/" separators, otherwise we have
   * a problem between platforms: maybe add an entry to set the separator *)
  let archive = Lib.Archive.Filename (expand_environment_variables archive) in
  let iq = expand_environment_variables in_ in
  let pq = expand_environment_variables p in
  if not (Sys.file_exists pq) then ignore (mkdir pq) else ();
  Lib.Archive.extract archive ~transform:Lib.Archive.Transform.(wrap [
    filter (Str.regexp_case_fold iq);
    strip_prefix_length (try 1 + String.rindex iq '/' with Not_found -> 0);
    c pq;
  ])

(* rm with verbose output
 *   doesn't fail if a file doesn't exist
 *   removes directories only if empty *)
let rm path_unexpanded =
  let exists path =
    (* Sys.file_exists and most others will follow symlinks This means that if
     * 'y' points to 'x' but 'x' doesn't exist, Sys.file_exists will return
     * false even though 'y' exists *)
    try ignore (Unix.lstat path); true with _ -> false
  in
  (* FIXME: env var souldn't be kept in the database, they have to be expanded
   * before *)
  let path_expanded = expand_environment_variables path_unexpanded in
  let path = FilePath.DefaultPath.reduce path_expanded in
  if exists path then
    if FileUtil.test FileUtil.Is_dir path then
      if [| |] = Sys.readdir path then
        let () = FileUtil.rm ~recurse:true [ path ] in
        Lib.ep "Removed: %s\n" path
      else
        Lib.ep "Not removed (directory not empty): %s\n" path
    else
      let () = FileUtil.rm [ path ] in
      Lib.ep "Removed: %s\n" path
  else
    Lib.ep "Not removed (doesn't exist): %s\n" path

(* checks if a file exists in any package in a given database *)
let file_exists_in_package file (_, result_list) =
  List.exists (fun (_id, l) -> List.mem file l) result_list

(* Return the yypkg metadata from a sherpa package *)
let metadata_of_pkg ((m, _, _), _) =
  m

let metadata_of_script (m, _, _) =
  m

(* a predicate to check a package has some name, used with List.find *)
let package_is_named name p =
  (metadata_of_pkg p).name = name

(* Find if a package by name, matching a regular exression *)
let find_all_by_name_regex db re =
  ListLabels.find_all db ~f:(fun p ->
    Str.string_match re (metadata_of_pkg p).name 0
  )

(* Test if a package is installed, by name *)
let is_installed db p =
  List.exists (package_is_named p) db

(* various sanity checks:
  * do etc/yypkg.conf and /var/log/packages/yypkg_db exist?
  * ... *)
let sanity_checks prefix =
  List.iter Lib.assert_file_exists [
    Filename.concat prefix db_path;
    Filename.concat prefix conf_path
  ]

let symlink ~target ~name ~kind =
  let log_unix_error (error, f, arg) =
    Lib.ep "Unix_error: `%s` `%s`: `%s`\n%!" f arg (Unix.error_message error)
  in
  let unlink f =
    Lib.(log dbg "Calling unlink(`%s').\n" f);
    try Unix.unlink f with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    | Unix.Unix_error (e, s1, s2) as x -> log_unix_error (e, s1, s2); raise x
  in
  let remove f =
    Lib.(log dbg "Calling remove(`%s').\n" f);
    try remove f with
    | Failure s  -> Lib.ep "Failure in remove: %s\n%!" s
  in
  let link target name =
    Lib.(log dbg "Calling link(`%s', `%s').\n" target name);
    try Unix.link target name with
    | Unix.Unix_error (e, s1, s2) as x -> log_unix_error (e, s1, s2); raise x
  in
  let symlink target name =
    Lib.(log dbg "Calling symlink(`%s', `%s').\n" target name);
    try Unix.symlink target name with
    | Unix.Unix_error (e, s1, s2) as x -> log_unix_error (e, s1, s2); raise x
  in
  match Lib.os_type, kind with
  | `Unix, _ -> unlink name; symlink target name
  | `Windows, `File ->
      (* FIXME: target_abs below will be wrong if target is an absolute path *)
      let target_abs = String.concat "/" [ FilePath.dirname name; target ] in
      remove name; link target_abs name
  | `Windows, `Directory ->
      remove name;
      let target_abs = Lib.filename_concat [ FilePath.dirname name; target ] in
      Lib.(log dbg "Calling create_reparse_point(`%s', `%s').\n" target_abs name);
      create_reparse_point target_abs name
  | `Windows, `Unhandled reason ->
      Lib.ep "Skipping symlink %S -> %S: %s\n" name target reason

(* check if the predicate holds against conf *)
let predicate_holds (conf : predicate list) (key, value) = 
  (* List.assoc may raise Not_found: means the predicate hasn't been set in the
   * configuration, equivalent to false *)
  try 
    let conf_vals = List.assoc key conf in
    List.mem value conf_vals
  with Not_found -> false

let find_all_by_name ~pkglist ~name_list =
  let open Types.Repo in
  List.filter (fun p -> List.mem p.metadata.name name_list) pkglist

let xz_opt size =
  let max_dict = 1 lsl 26 in (* 64MB *)
  let min_dict = 1 lsl 18 in (* 256kB *)
  let smallest_bigger_power_of_two size =
    let log = Pervasives.log in
    2 lsl (int_of_float (log (Int64.to_float size) /. (log 2.)))
  in
  let lzma_settings ~fastest size =
    if fastest then
      Lib.sp "dict=%d,mf=%s,mode=%s,nice=%d" min_dict "hc3" "fast" 3
    else
      let dict = max min_dict (smallest_bigger_power_of_two size) in
      let dict = min max_dict dict in
      Lib.sp "dict=%d,mf=%s,mode=%s,nice=%d" dict "bt4" "normal" 128
  in
  (* YYLOWCOMPRESS is mostly a quick hack, no need to make it very clean *)
  let fastest = try Sys.getenv "YYLOWCOMPRESS" != "" with _ -> false in
  let lzma_settings = lzma_settings ~fastest size in
  String.concat " " [ "-vv"; "--x86"; Lib.sp "--lzma2=%s" lzma_settings ]

(* tar + xz *)
let tar_xz ~tar_args ~xz_opt ~out =
  let module U = Unix in
  let argv = Array.concat (
    [| "bsdtar"; "cvf"; out; "--use-compress-program"; "xz" |]
    :: tar_args
  ) in
  let env = Array.concat [ [| "XZ_OPT=" ^ xz_opt |]; U.environment () ] in
  let pid = U.create_process_env argv.(0) argv env U.stdin U.stdout U.stderr in
  match U.waitpid [] pid with
  | _, U.WEXITED 0 -> ()
  | _ -> Lib.process_failed argv

