(*
 * makeypkg - A program to ease the creation of yypkg packages
 * Copyright (C) 2010 Adrien Nader
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
open Lib
open Types

exception Package_name_must_end_in_txz (* XXX: not used currently *)

type settings = {
  output : string;
  folder : string;
  folder_dirname : string;
  folder_basename : string;
  metafile : string;
}

let rec strip_trailing_slash s =
  (* dir_sep's length is 1 *)
  if s.[String.length s - 1] = dir_sep.[0] then
    strip_trailing_slash (String.sub s 0 (String.length s - 1))
  else
    s

let output_file { name; version; predicates } =
  let arch = arch_of_preds predicates in
  (* if we included the ext in concat's call, we'd have an extra separator *)
  (String.concat "-" [ name; string_of_version version; arch ]) ^ ".txz"

let parse_command_line () = 
  let output, folder, meta = ref "", ref "", ref "" in
  let lst = [
    (* the output file*name* will be built from the other param values *)
    "-o", Arg.Set_string output, "output folder";
    "-meta", Arg.Set_string meta, "package metadata file";
  ]
  in
  let usage_msg = "ERROR: All arguments mentionned in --help are mandatory." in
  let no_meta_msg = "WARNING: No meta file given, using (stupid) defaults." in
  (* the last argument is the folder to package *)
  let () = Arg.parse lst ((:=) folder) usage_msg in
  (* check if any argument has not been set (missing from the command-line *)
  if List.exists ((=) "") [ !output; !folder; !meta ] then
    let () = prerr_endline usage_msg in
    exit 0
  else
    (if !meta = "" then prerr_endline no_meta_msg);
    let folder = strip_trailing_slash !folder in
    let dirname = FilePath.DefaultPath.dirname (
      if not (FilePath.DefaultPath.is_relative folder) then folder
      else FilePath.DefaultPath.make_absolute (Sys.getcwd ()) folder )
    in
    {
      output = !output;
      folder = folder;
      folder_dirname = dirname;
      folder_basename = FilePath.DefaultPath.basename folder;
      metafile = !meta;
    }

let meta ~metafile ~pkg_size =
  let metadata = metadata_of_sexp (Sexplib.Sexp.load_sexp metafile) in
  { metadata with size_expanded = pkg_size }

let prefix_of_arch = function (* XXX: quite often, we might get "noarch" as an
arch, so we definitely do need to take that case into account *)
  | "i686-w64-mingw32"
  | "x86_64-w64-mingw32" as s -> s
  | "i686-pc-mingw32" -> "/mingw"
  | _ -> assert false (* XXX *)

module PrefixFix = struct
  (* Some files (.pc for pkgconfig and .la for libtool for instance) contain
   * hard-coded paths. We find them and replace them with a variable which value
   * will be set during package install. Not perfect but usually works. *)
  let find_files folder ext =
    FileUtil.find (FileUtil.Has_extension ext) folder (fun x y -> y :: x) []

  let install_actions folder file =
    let file = split_path (FilePath.make_relative folder file) in
    let file = String.concat " " file in
    "dummy", SearchReplace ([file], "__YYPREFIX", "${YYPREFIX}")

  let find_prefix prefix_re file =
    let contents = read_file file in
    let l = Queue.fold (fun l x -> x :: l) [] contents in
    let prefix = List.find (fun s -> Str.string_match prefix_re s 0) l in
    (* matched_group will get the match from the List.find line before *)
    Str.matched_group 1 prefix

  let fix_file arch ext prefix_re fix file =
    let prefix = find_prefix prefix_re file in
    let new_prefix = "__YYPREFIX/" ^ (prefix_of_arch arch) in
    fix file prefix new_prefix

  let fix_files arch folder ext prefix_re fix =
    let files = find_files folder ext in
    List.iter (fix_file arch ext prefix_re fix) files;
    List.map (install_actions folder) files
end

let pkg_config_fixup folder arch = 
  let f file prefix new_prefix = 
    search_and_replace_in_file file prefix "${prefix}";
    search_and_replace_in_file file "^prefix=\\${prefix}" ("prefix="^new_prefix)
  in
  let prefix_re = Str.regexp "^prefix=\\(.*\\)" in
  PrefixFix.fix_files arch folder "pc" prefix_re f

let libtool_fixup folder arch =
  let f file prefix new_prefix =
    let strip_slashes_re, strip_slashes_repl = "//+", "/" in
    let simplify_path_re, simplify_path_repl = "\\(|^/]+/../\\)", "" in
    let prefix_re = "[^'=]*" ^ prefix in
    search_and_replace_in_file file strip_slashes_re strip_slashes_repl;
    search_and_replace_in_file file simplify_path_re simplify_path_repl;
    search_and_replace_in_file file prefix_re new_prefix
  in
  let libdir_re = Str.regexp "libdir='\\(.*\\).lib.*'" in
  PrefixFix.fix_files arch folder "la" libdir_re f

let path_fixups folder arch fixups =
  let dispatch folder arch = function
    | `PkgConfig -> pkg_config_fixup folder arch
    | `Libtool -> libtool_fixup folder arch
  in
  List.concat (List.map (dispatch folder arch) fixups)

let package_script_el ~pkg_size { folder_basename; metafile; folder } =
  let folder = folder_basename in
  let meta = meta ~metafile ~pkg_size in
  let arch = arch_of_preds meta.predicates in
  let expand = folder, Expand (folder, ".") in
  let path_fixups = path_fixups folder arch [ `PkgConfig; `Libtool ] in
  meta, (expand :: path_fixups), [ Reverse folder ]

let smallest_bigger_power_of_two size =
  2 lsl (int_of_float (log (Int64.to_float size) /. (log 2.)))

let xz_call size =
  let lzma_settings size = String.concat "," [
    sprintf "dict=%d" (max (1 lsl 26) (smallest_bigger_power_of_two size));
    "lc=3"; "lp=0"; "pb=2"; "mode=normal"; "nice=64"; "mf=bt4"; "depth=0";
  ]
  in
  [| xz; "--x86"; sprintf "--lzma2=%s" (lzma_settings size) |]

let compress settings meta (script_dir, script_name) =
  let tar_args = [| "-C"; script_dir; script_name; "-C"; settings.folder_dirname; settings.folder_basename |] in
  let snd = xz_call (FileUtil.byte_of_size meta.size_expanded) in
  let output_file = output_file meta in
  let output_path = Filename.concat settings.output output_file in
  tar_compress tar_args snd output_path;
  output_path

let () =
  let settings = parse_command_line () in
  let pkg_size = fst (FileUtil.du [ settings.folder ]) in
  let meta, _, _ as script = package_script_el ~pkg_size settings in
  let script = Sexplib.Sexp.to_string_hum (sexp_of_script script) in
  let script_dir_and_name = write_temp_file "package_script.el" script in
  let output_file = compress settings meta script_dir_and_name in
  Printf.printf "Package created in: %s\n." output_file

