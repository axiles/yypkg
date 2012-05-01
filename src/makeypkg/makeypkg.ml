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

open Types
open Lib
open Types

let sp = Printf.sprintf

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
    let p = sp "%s=%s" in
    String.concat "," [ p "dict" dict; p "mf" mf; p "mode" mode; p "nice" nice ]
  in
  (* YYLOWCOMPRESS is mostly a quick hack, no need to make it very clean *)
  let fastest = try Sys.getenv "YYLOWCOMPRESS" != "" with _ -> false in
  let lzma_settings = lzma_settings ~fastest size in
  [| xz; "-vv"; "--x86"; sp "--lzma2=%s" lzma_settings |]

let rec strip_trailing_slash s =
  (* dir_sep's length is 1 *)
  if s <> "" && s.[String.length s - 1] = dir_sep.[0] then
    strip_trailing_slash (String.sub s 0 (String.length s - 1))
  else
    s

type dir = {
  path : string;
  dirname : string;
  basename : string;
}

type settings = {
  output : string;
  package : dir;
  install_scripts : dir option;
  metafile : string;
}

let dir_of_path path =
  let module FP = FilePath in
  let abs_path =
    let dir = strip_trailing_slash path in
    if FP.is_relative dir then
      FP.make_absolute (Sys.getcwd ()) dir
    else
      dir
  in
  { path = path; dirname = FP.dirname abs_path; basename = FP.basename path }

module Package_script_el = struct
  let meta ~metafile ~pkg_size =
    let sexp = match metafile with
    | "-" -> Sexplib.Sexp.input_sexp stdin
    | file -> Sexplib.Sexp.load_sexp file
    in
    { (TypesSexp.To.metadata sexp) with size_expanded = pkg_size }

  let run_iscripts = function
    | Some dir ->
        (* FIXME: handle .ahk scripts *)
        let accumulate l e =
          let path = FilePath.concat dir.path e in
          let path_in = FilePath.UnixPath.concat dir.basename e in
          if FilePath.get_extension path = "sh"
            && FileUtil.test FileUtil.Is_file path
            && FileUtil.test FileUtil.Is_exec path
          then (e, Exec [ path_in ]) :: l else l
        in
        List.sort compare (Array.fold_left accumulate [] (Sys.readdir dir.path))
    | None -> []

  let make ~pkg_size settings =
    let dir = settings.package.basename in
    let meta = meta ~metafile:settings.metafile ~pkg_size in
    (* we want to expand the content of dir so we suffix it with '/' *)
    let expand = dir, Expand (dir ^ "/", ".") in
    meta, expand :: (run_iscripts settings.install_scripts), [ Reverse dir ]
end

let output_file meta =
  let version = string_of_version meta.version in
  match meta.target with
  | None -> sp "%s-%s-%s.txz" meta.name version meta.host
  | Some target -> sp "%s-%s-%s-%s.txz" meta.name version target meta.host

let compress settings meta (script_dir, script_name) =
  let install_scripts = function 
    | Some dir -> [| "-C"; dir.dirname; dir.basename |]
    | None -> [| |]
  in
  let tar_args = Array.concat [
    [| "-C"; script_dir; script_name |];
    install_scripts settings.install_scripts;
    [| "-C"; settings.package.dirname; settings.package.basename |]
  ]
  in
  let snd = xz_call (FileUtil.byte_of_size meta.size_expanded) in
  let output_path = Filename.concat settings.output (output_file meta) in
  tar_compress tar_args snd output_path;
  output_path

let dummy_meta () =
  let version = dummy_version () in
  let size_expanded = FileUtil.TB (Int64.of_int 42) in
  let meta = { name = "dummy_name"; size_expanded = size_expanded; version =
    version; packager_email = "adrien@notk.org"; packager_name = "Adrien Nader";
    description = "dummy"; host = "%{HST}"; target = Some "%{TGT}";
    predicates = []; comments = [] }
  in
  Sexplib.Sexp.to_string_hum (TypesSexp.Of.metadata meta)

let parse_command_line () = 
  let output, dir, iscripts, meta, template =
    ref (Sys.getcwd ()), ref "", ref "", ref "", ref false in
  let lst = [
    (* the output file*name* will be built from the other param values *)
    "-o", Arg.Set_string output, "output directory (defaults to current dir)";
    "-meta", Arg.Set_string meta, "package metadata file (- for stdin)";
    "-iscripts", Arg.Set_string iscripts, "directory of install scripts";
    "-template", Arg.Set template, "write a template meta on stdout";
  ]
  in
  let usage_msg = "\
Create a yypkg package from a directory.
Use either (-o, -meta, -iscripts and a directory) XOR -template (see -help).
Examples:
  $ makeypkg -o /some/dir -meta pcre.META -iscripts iscripts-atk package-atk
  $ makeypkg -template"
    in
  (* the last argument is the directory to package *)
  Arg.parse lst ((:=) dir) usage_msg;
  if !template then
    (print_endline (dummy_meta ()); exit 0)
  else
    (* check if any argument has not been set (missing from the command-line *)
    if List.mem "" [ !output; !dir; !meta ] then
      let () = prerr_endline usage_msg in
      exit (-1)
    else
      {
        output = !output;
        package = dir_of_path !dir;
        install_scripts =
          if !iscripts <> "" then Some (dir_of_path !iscripts) else None;
        metafile = !meta;
      }

let () =
  let settings = parse_command_line () in
  let pkg_size = fst (FileUtil.du [ settings.package.path ]) in
  let meta, _, _ as script = Package_script_el.make ~pkg_size settings in
  let script = Sexplib.Sexp.to_string_hum (TypesSexp.Of.script script) in
  let script_dir_and_name = write_temp_file "package_script.el" script in
  let output_file = compress settings meta script_dir_and_name in
  Printf.printf "Package created as: %s\n." output_file

