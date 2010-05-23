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

exception Package_name_must_end_in_txz_tgz_or_tbz2

type cmd_line = {
  output : string;
  folder : string;
  folder_dirname : string;
  folder_basename : string;
  pkg_name : string;
  version : version;
  pkger_name : string;
  pkger_email : string;
  descr : string;
  arch : string;
  compressor : string;
}

let strip_trailing_slash s =
  (* dir_sep's length is 1 *)
  if s.[String.length s - 1] = dir_sep.[0] then
    String.sub s 0 (String.length s - 1)
  else
    s

let output_of_cmdline c =
  let ext = match c.compressor with
    | "xz"  -> ".txz"
    | "gzip" -> ".tgz"
    | "bzip2" -> ".tbz2"
    | _ -> assert false
  in
  (* if we put 'ext' in concat's call, we'd have an extra separator ("-") *)
  (String.concat "-" [ c.pkg_name; string_of_version c.version; c.arch ]) ^ ext

let parse_command_line () = 
  let output,folder,pkg_name,version,packager_email,packager_name,description,arch, compressor =
    ref "", ref "", ref "", ref "", ref "", ref "", ref "", ref "", ref "xz"
  in
  let lst = [
    (* the output filename will be made from the other param values *)
    "-o", Arg.Set_string output, "set output folder";
    "-name", Arg.Set_string pkg_name, "set the package name";
    "-version", Arg.Set_string version, "set the package version";
    "-email", Arg.Set_string packager_email, "packager's email";
    "-packager_name", Arg.Set_string packager_name, "packager's name";
    "-description", Arg.Set_string description, "description";
    (* the package will only install if arch is matched *)
    "-arch", Arg.Set_string arch, "arch triplet";
    "-compressor", Arg.Set_string compressor, "xz, gzip or bzip2";
  ]
  in
  let usage_msg = "All arguments mentionned in --help are mandatory." in
  (* the last argument is the folder to package *)
  let () = Arg.parse lst ((:=) folder) usage_msg in
  (* check if any argument has not been set (missing from the command-line *)
  if List.exists ((=) (ref "")) [output; folder; pkg_name; version; packager_name; packager_email; description; arch; compressor] then
    let () = prerr_endline usage_msg in
    exit 0
  else
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
      pkg_name = !pkg_name;
      version = version_of_string !version;
      pkger_name = !packager_name;
      pkger_email = !packager_email;
      descr = !description;
      arch = !arch;
      compressor = !compressor;
    }

let meta ~cmd_line ~pkg_size =
  String.concat "\n" [
    sprintf "(description \"%s\")" cmd_line.descr;
    sprintf "(package_name \"%s\")" cmd_line.pkg_name;
    sprintf "(package_version \"%s\")" (string_of_version cmd_line.version);
    sprintf "(package_size_expanded \"%s\")" pkg_size;
    sprintf "(packager_name \"%s\")" cmd_line.pkger_name;
    sprintf "(packager_email \"%s\")" cmd_line.pkger_email;
    sprintf "(predicates ((\"%s\" \"%s\")))" "arch" cmd_line.arch;
    sprintf "(comments \"made with makeypkg\")";
  ]

let prefix_of_arch = function
  | "i686-w64-mingw32"
  | "x86_64-w64-mingw32" as s -> s ^ "/mingw"
  | "i686-pc-mingw32" -> "/mingw"
  | _ -> assert false

let path_fixups folder arch fixups =
  let find_per_ext ext =
    FileUtil.find (FileUtil.Has_extension ext) folder (fun x y -> y :: x) []
  in
  let pkg_config_fixup () = 
    let file_fixup file =
      let prefix_re = Str.regexp "prefix=\\(.*\\)" in
      let contents = read_file file in
      let l = Queue.fold (fun l x -> x :: l) [] contents in
      let prefix = List.find (fun s -> Str.string_match prefix_re s 0) l in
      let prefix = Str.replace_first prefix_re "\\1" prefix in
      let new_prefix = "__YYPREFIX/" ^ (prefix_of_arch arch) in
      search_and_replace_in_file file prefix "${prefix}";
      search_and_replace_in_file file "prefix=\\${prefix}" ("prefix="^new_prefix)
    in
    let dot_pc_files = find_per_ext "pc" in
    List.iter file_fixup dot_pc_files;
    dot_pc_files
  in
  let pkg_config_search_replace f =
    sprintf "(\"dummy\" (SearchReplace %s __YYPREFIX ${YYPREFIX}))" f
  in
  let dot_pc_files = pkg_config_fixup () in
  let pc_fixups = List.map pkg_config_search_replace dot_pc_files in
  pc_fixups

let package_script_el cmd_line ~pkg_size =
  let folder = cmd_line.folder_basename in
  let meta = meta ~cmd_line ~pkg_size in
  let expand = sprintf "(\"%s\" (Expand \"%s/*\" \"%s\"))" folder folder "." in
  let path_fixups = path_fixups cmd_line.folder cmd_line.arch [ `PkgConfig ] in
  let install = String.concat "\n" (expand :: path_fixups) in
  let uninstall = sprintf "(Reverse \"%s\")" folder in
  let l = List.map (sprintf "(\n%s\n)") [ meta; install; uninstall ] in
  sprintf "(\n%s\n)" (String.concat "\n" l)

let () =
  let cmd_line = parse_command_line () in
  let pkg_size= FileUtil.string_of_size (fst (FileUtil.du [cmd_line.folder])) in
  let script = package_script_el ~pkg_size cmd_line in
  let script_dir, script_name = write_temp_file "package_script.el" script in
  let tar_args = [| "-C"; script_dir; script_name; "-C"; cmd_line.folder_dirname; cmd_line.folder_basename |] in
  let snd = match cmd_line.compressor with
    | "xz" -> [| xz; "--x86"; "--lzma2=dict=67108864,lc=3,lp=0,pb=2,mode=normal,nice=64,mf=bt4,depth=0" |]
    | "gzip" -> [| gzip; "-9" |]
    | "bzip2" -> [| bzip2; "-9" |]
    | _ -> assert false
  in
  let output = Filename.concat cmd_line.output (output_of_cmdline cmd_line) in
  tar_compress tar_args snd output
