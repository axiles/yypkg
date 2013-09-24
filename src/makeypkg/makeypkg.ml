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

let xz_opt size =
  let max_dict = 1 lsl 26 in (* 64MB *)
  let min_dict = 1 lsl 18 in (* 256kB *)
  let smallest_bigger_power_of_two size =
    2 lsl (int_of_float (log (Int64.to_float size) /. (log 2.)))
  in
  let lzma_settings ~fastest size =
    if fastest then
      sp "dict=%d,mf=%s,mode=%s,nice=%d" min_dict "hc3" "fast" 3
    else
      let dict = max min_dict (smallest_bigger_power_of_two size) in
      let dict = min max_dict dict in
      sp "dict=%d,mf=%s,mode=%s,nice=%d" dict "bt4" "normal" 128
  in
  (* YYLOWCOMPRESS is mostly a quick hack, no need to make it very clean *)
  let fastest = try Sys.getenv "YYLOWCOMPRESS" != "" with _ -> false in
  let lzma_settings = lzma_settings ~fastest size in
  String.concat " " [ "-vv"; "--x86"; sp "--lzma2=%s" lzma_settings ]

(* tar + xz *)
let tar_xz tar_args xz_opt out =
  let module U = Unix in
  let tar_args = Array.concat
    ([| tar; "cvf"; out; "--use-compress-program"; xz |] :: tar_args) in
  let env = Array.concat [ [| "XZ_OPT=" ^ xz_opt |]; U.environment () ] in
  let pid = U.create_process_env tar tar_args env U.stdin U.stdout U.stderr in
  match U.waitpid [] pid with
  | _, U.WEXITED 0 -> ()
  | _ -> process_failed tar_args

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
  script : string;
}

let dir_of_path path =
  let module FP = FilePath in
  let path =
    let dir = strip_trailing_slash path in
    if FP.is_relative dir then
      FP.make_absolute (Sys.getcwd ()) dir
    else
      dir
  in
  { path; dirname = FP.dirname path; basename = FP.basename path }

module Package_script = struct
  let script ~script ~pkg_size =
    let sexp = match script with
    | "-" -> Sexplib.Sexp.input_sexp stdin
    | file -> Sexplib.Sexp.load_sexp file
    in
    let metadata, install, uninstall = TypesSexp.To.script sexp in
    { metadata with size_expanded = pkg_size }, install, uninstall

  let run_install_scripts dir =
    let aux dir =
      (* FIXME: handle .ahk scripts *)
      let accumulate l e =
        let path = FilePath.concat dir.path e in
        if
          try (FilePath.get_extension path = "sh") with Not_found -> false
          && FileUtil.test FileUtil.Is_file path
          && FileUtil.test FileUtil.Is_exec path
        then
          let path_in = Lib.filename_concat [ dir.basename; e ] in
          let dir_out = Lib.filename_concat [ Yylib.db_folder; dir.basename ] in
          let path_out = Lib.filename_concat [ dir_out; e ] in
          (e ^ "_pre", Expand (path_in, dir_out))
          :: (e, Exec [ "bash"; path_out ])
          :: l
        else
          l
      in
      let scripts = Sys.readdir dir.path in
      Array.sort compare scripts;
      Array.fold_left accumulate [] scripts
    in
    match dir with
    | Some dir -> aux dir
    | None -> []

  let segregate_symlinks dir =
    let module FU = FileUtil in
    let accumulate =
      let i = ref (-1) in
      fun l e ->
        let target = FU.readlink e in
        let kind = match (FU.stat target).FU.kind with
        | FU.Dir -> `Directory
        | FU.File -> `File
        | _ -> assert false (* other kinds don't make sense in packages *)
        in
        incr i;
        (sp "symlink_%d" !i, Symlink (target, e, kind)) :: l
    in
    FU.find ~follow:FU.Skip FU.Is_link dir accumulate []

  let build ~pkg_size settings =
    let dir = settings.package.basename in
    match script ~script:settings.script ~pkg_size with
    | meta, [], [] ->
        let expand_id = sp "expand_%s" dir in
        let install_actions =
          (* we want to expand the content of dir so we suffix it with '/' *)
          (expand_id, Expand (dir ^ "/", "."))
          :: run_install_scripts settings.install_scripts
          @ (segregate_symlinks settings.package.path)
        in
        meta, install_actions, [ Reverse "symlinks"; Reverse expand_id ]
    | script -> script
end

let output_file meta =
  let version = string_of_version meta.version in
  match meta.target with
  | None -> sp "%s-%s-%s.txz" meta.name version meta.host
  | Some target -> sp "%s-%s-%s-%s.txz" meta.name version target meta.host

let archive settings (meta, install_actions, _) (script_dir, script_name) =
  let symlinks =
    list_rev_map_skip install_actions ~f:(function
      | _id, Symlink (_t, n, _k) -> sp "--exclude=%s" n
      | _ -> raise Skip
    )
  in
  let install_scripts = function 
    | Some dir -> [| "-C"; dir.dirname; dir.basename |]
    | None -> [| |]
  in
  let tar_args = [
    [| "--exclude=*.la" |];
    Array.of_list symlinks;
    [| "-C"; script_dir; script_name |];
    install_scripts settings.install_scripts;
    [| "-C"; settings.package.dirname; settings.package.basename |]
  ]
  in
  let xz_opt = xz_opt (FileUtil.byte_of_size meta.size_expanded) in
  let output_path = Filename.concat settings.output (output_file meta) in
  tar_xz tar_args xz_opt output_path;
  output_path

let dummy_script () =
  let version = dummy_version () in
  let size_expanded = FileUtil.TB (Int64.of_int 42) in
  let metadata = { name = "dummy_name"; size_expanded; version;
    packager_email = "foo@bar.com"; packager_name = "Adrien Nader";
    description = "dummy"; host = "%{HST}"; target = Some "%{TGT}";
    predicates = []; comments = [] }
  in
  Sexplib.Sexp.to_string_hum (TypesSexp.Of.script (metadata, [], []))

let parse_command_line () = 
  let output, dir, iscripts, script, template =
    ref (Sys.getcwd ()), ref "", ref "", ref "", ref false in
  let lst = [
    (* the output file*name* will be built from the other param values *)
    "-o", Arg.Set_string output, "output directory (defaults to current dir)";
    "-script", Arg.Set_string script, "package script file (- for stdin)";
    "-iscripts", Arg.Set_string iscripts, "directory of install scripts";
    "-template", Arg.Set template, "write a template script on stdout";
  ]
  in
  let usage_msg = "\
Create a yypkg package from a directory.
Use either (-o, -script, -iscripts and a directory) XOR -template (see -help).
Examples:
  $ makeypkg -o /some/dir -script pcre.META -iscripts iscripts-atk package-atk
  $ makeypkg -template"
    in
  (* the last argument is the directory to package *)
  Arg.parse lst ((:=) dir) usage_msg;
  if !template then
    (print_endline (dummy_script ()); exit 0)
  else
    (* check if any argument has not been set (missing from the command-line *)
    if List.mem "" [ !output; !dir; !script ] then
      let () = prerr_endline usage_msg in
      exit (-1)
    else
      {
        output = !output;
        package = dir_of_path !dir;
        install_scripts =
          if !iscripts <> "" then Some (dir_of_path !iscripts) else None;
        script = !script;
      }

let () =
  let settings = parse_command_line () in
  let pkg_size = fst (FileUtil.du [ settings.package.path ]) in
  let script = Package_script.build ~pkg_size settings in
  let script_sexp = Sexplib.Sexp.to_string_hum (TypesSexp.Of.script script) in
  let script_dir_and_name = write_temp_file "package_script.el" script_sexp in
  let output_file = archive settings script script_dir_and_name in
  Printf.printf "Package created as: %s\n." output_file

