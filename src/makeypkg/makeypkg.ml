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

let prefix_arch = [
  "i686-w64-mingw32", "i686-w64-mingw32";
  "x86_64-w64-mingw32", "x86_64-w64-mingw32";
]

let xz_call size =
  let sixty_four_mb = 1 lsl 26 in (* max xz dictionnary size *)
  let four_kb = 1 lsl 12 in (* min xz dictionnary size *)
  let smallest_bigger_power_of_two size =
    2 lsl (int_of_float (log (Int64.to_float size) /. (log 2.)))
  in
  let lzma_settings ?(fastest=false) size =
    let dict = max four_kb (smallest_bigger_power_of_two size) in
    let dict = min sixty_four_mb dict in
    let dict, mf, mode, nice = if fastest then
      string_of_int four_kb, "hc3", "fast", "3"
    else
      string_of_int dict, "bt4", "normal", "128"
    in
    let p = sprintf "%s=%s" in
    String.concat "," [ p "dict" dict; p "mf" mf; p "mode" mode; p "nice" nice ]
  in
  (* yylowcompress is mostly a quick hack, no need to make it very clean *)
  let fastest = try Sys.getenv "YYLOWCOMPRESS" != "" with _ -> false in
  let lzma_settings = lzma_settings ~fastest size in
  [| xz; "-vv"; "--x86"; sprintf "--lzma2=%s" lzma_settings |]

let rec strip_trailing_slash s =
  (* dir_sep's length is 1 *)
  if s.[String.length s - 1] = dir_sep.[0] then
    strip_trailing_slash (String.sub s 0 (String.length s - 1))
  else
    s

module PrefixFix = struct
  (* Some files (.pc for pkgconfig and .la for libtool for instance) contain
   * hard-coded paths. We find them and replace them with a variable which value
   * will be set during package install. Not perfect but usually works. *)
  let find_files folder ext =
    FileUtil.find (FileUtil.Has_extension ext) folder (fun x y -> y :: x) []

  let install_actions folder file =
    (* we need to find the path relative to the package root
     * actual example:
       * file is /tmp/package-gettext/i686-w64-mingw32/lib/libintl.la
       * folder is /tmp/package-gettext *)
    let file = split_path file in
    let folder = split_path folder in
    let file = chop_list file (List.length folder) in
    let file = String.concat "/" file in
    "dummy", SearchReplace ([file], "__YYPREFIX", "${YYPREFIX}")

  let find_prefix prefix_re file =
    let contents = read_file file in
    let l = Queue.fold (fun l x -> x :: l) [] contents in
    let prefix = List.find (fun s -> Str.string_match prefix_re s 0) l in
    (* matched_group will get the match from the List.find line before *)
    Str.matched_group 1 prefix

  let fix_file prefix prefix_re fix file =
    (* it's possible that we don't find the prefix, raising an exception *)
    try
      let prefix = find_prefix prefix_re file in
      (* We always want '/' as a path separator *)
      let new_prefix = "__YYPREFIX/" ^ prefix in
      fix ~file ~prefix ~new_prefix
    with Not_found -> ()

  let fix_files ~prefix ~folder ~ext ~search_re ~fix =
    let files = find_files folder ext in
    List.iter (fix_file prefix search_re fix) files;
    List.map (install_actions folder) files
end

exception Package_name_must_end_in_txz (* XXX: not used currently *)

type settings = {
  output : string;
  folder : string;
  folder_dirname : string;
  folder_basename : string;
  metafile : string;
}

let output_file metadata =
  let arch = arch_of_preds metadata.predicates in
  (* if we included the ext in concat's call, we'd have an extra separator *)
  sprintf "%s-%s-%s.txz" metadata.name (string_of_version metadata.version) arch

let meta ~metafile ~pkg_size =
  let sexp = match metafile with
  | "-" -> Sexplib.Sexp.input_sexp stdin
  | file -> Sexplib.Sexp.load_sexp file
  in
  { metadata_of_sexp sexp with size_expanded = pkg_size }

let pkg_config_fixup ~folder ~prefix = 
  let fix ~file ~prefix ~new_prefix = 
    search_and_replace_in_file file prefix "${prefix}";
    search_and_replace_in_file file "^prefix=\\${prefix}" ("prefix="^new_prefix)
  in
  let search_re = Str.regexp "^prefix=\\(.*\\)" in
  PrefixFix.fix_files ~prefix ~folder ~ext:"pc" ~search_re ~fix

let libtool_fixup ~folder ~prefix =
  let fix ~file ~prefix ~new_prefix =
    (* Replace "foo///////bar///" with only "foor/bar/" *)
    let strip_slashes_re, strip_slashes_repl = "/+", "/" in
    (* Replace "foo/../bar" with "bar" *)
    let simplify_path_re, simplify_path_repl = "\\([^/']+/+\\.\\.\\)", "" in
    (* We set prefix to /foo/bar/x86_64-w64-mingw32/ during compilation but want
     * to replace it with ${YYPREFIX}/x86_64-w64-mingw32/ : we have to include
     * the "/foo/bar/" part too in the match expression *)
    let prefix_re = "\\([ '=]\\)" ^ prefix in
    (* When the installation prefix is /i686-w64-mingw32, we can get strings
     * like /i686-w64-mingw32/i686-w64-mingw32/lib32 and we need to make them
     * independant of the installation prefix so we change the first occurence
     * of "/i686-w64-mingw32" with "__YYPREFIX" *)
    (* XXX: when the installation prefix is *NOT* that, we still have to do
      * something, like: ${FOO}/i686-w64-mingw32/lib32 -> s/${FOO/__YYPREFIX/ *)
    let install_prefix_is_prefix_re = prefix_re ^ prefix in
    let change_prefix = "\\1__YYPREFIX" ^ prefix in
    (* With the previous step done, the strings we still have (can) change are
     * of the form "/i686-w64-mingw32/lib32" and for these, we prefix __YYPREFIX
     * to them *)
    let prepend_prefix = "\\1" ^ new_prefix in
    search_and_replace_in_file file simplify_path_re simplify_path_repl;
    search_and_replace_in_file file strip_slashes_re strip_slashes_repl;
    search_and_replace_in_file file install_prefix_is_prefix_re change_prefix;
    search_and_replace_in_file file prefix_re prepend_prefix
  in
  let search_re = Str.regexp "libdir='\\(.*\\)/+lib.*'" in
  PrefixFix.fix_files ~prefix ~folder ~ext:"la" ~search_re ~fix

let path_fixups folder arch fixups =
  (* If the arch is unknown or is "noarch", we have to disable auto-fixes *)
  if List.mem_assoc arch prefix_arch then
    let prefix = List.assoc arch prefix_arch in
    let dispatch folder prefix = function
      | `PkgConfig -> pkg_config_fixup ~folder ~prefix
      | `Libtool -> libtool_fixup ~folder ~prefix
    in
    List.concat (List.map (dispatch folder prefix) fixups)
  else
    []

let package_script_el ~pkg_size settings =
  let folder = settings.folder_basename in
  let meta = meta ~metafile:settings.metafile ~pkg_size in
  let arch = arch_of_preds meta.predicates in
  (* we want to expand the content of folder so we suffix it with '/' *)
  let expand = folder, Expand (folder ^ "/", ".") in
  let path_fixups = path_fixups settings.folder arch [ `PkgConfig; `Libtool ] in
  meta, (expand :: path_fixups), [ Reverse folder ]

let compress settings meta (script_dir, script_name) =
  let tar_args = [| "-C"; script_dir; script_name; "-C"; settings.folder_dirname; settings.folder_basename |] in
  let snd = xz_call (FileUtil.byte_of_size meta.size_expanded) in
  let output_file = output_file meta in
  let output_path = Filename.concat settings.output output_file in
  tar_compress tar_args snd output_path;
  output_path

let parse_command_line () = 
  let output, folder, meta, template = ref "", ref "", ref "", ref false in
  let lst = [
    (* the output file*name* will be built from the other param values *)
    "-o", Arg.Set_string output, "output folder (defaults to current dir)";
    "-meta", Arg.Set_string meta, "package metadata file";
    "-template", Arg.Set template, "write a template meta on stdout";
  ]
  in
  let usage_msg = "\
Create a yypkg package from a folder.
Use either (-o, -meta and a folder) XOR -template (see -help). Examples:
  $ makeypkg -o /some/folder -meta pcre.META pcre-1.2.3
  $ makeypkg -template"
    in
  (* the last argument is the folder to package *)
  Arg.parse lst ((:=) folder) usage_msg;
  if !template then
    (print_endline (dummy_meta ()); exit 0)
  else
    (* check if any argument has not been set (missing from the command-line *)
    if List.mem "" [ !output; !folder; !meta ] then
      let () = prerr_endline usage_msg in
      exit (-1)
    else
      let folder = strip_trailing_slash !folder in
      (* make 'folder' an absolute path *)
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

let () =
  let settings = parse_command_line () in
  let pkg_size = fst (FileUtil.du [ settings.folder ]) in
  let meta, _, _ as script = package_script_el ~pkg_size settings in
  let script = Sexplib.Sexp.to_string_hum (sexp_of_script script) in
  let script_dir_and_name = write_temp_file "package_script.el" script in
  let output_file = compress settings meta script_dir_and_name in
  Printf.printf "Package created in: %s\n." output_file

