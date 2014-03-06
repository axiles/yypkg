(*
 * makeypkg - A program to ease the creation of yypkg packages
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
open Lib

let xz_opt size =
  let max_dict = 1 lsl 26 in (* 64MB *)
  let min_dict = 1 lsl 18 in (* 256kB *)
  let smallest_bigger_power_of_two size =
    let log = Pervasives.log in
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
  (* dir_sep's length is always 1 *)
  if s <> "" && s.[String.length s - 1] = Filename.dir_sep.[0] then
    strip_trailing_slash (String.sub s 0 (String.length s - 1))
  else
    s

type dir = {
  path : string;
  dirname : string;
  basename : string;
}

type makepkg_opts = {
  output : string;
  script : string;
  install_scripts : dir option;
  directory : dir;
  template : bool;
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
    | "-" -> Pre_sexp.input_sexp stdin
    | file -> Pre_sexp.load_sexp file
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
    let report_format = format_of_string "[symlink] %s: %S -> %S\n" in
    let report e t kind =
      let kind_string = match kind with
      | `Directory -> "Directory"
      | `File -> "File"
      | `Unhandled reason -> sp "Unhandled (%s)" reason
      in
      ep report_format kind_string e t
    in
    let accumulate =
      fun l e0 ->
        let target0 = Unix.readlink e0 in
        let e = FilePath.make_relative dir e0 in
        let target = FilePath.make_relative (FilePath.dirname e0) target0 in
        let kind =
          try
            match Unix.((stat e0).st_kind) with
            | Unix.S_DIR -> `Directory
            | Unix.S_REG -> `File
            | _ -> `Unhandled "File neither directory nor file"
          with Unix.Unix_error (Unix.ENOENT, _, _) ->
            `Unhandled "ENOENT"
        in
        report e target kind;
        ("symlink", (target, e, kind)) :: l
    in
    let links = FU.find ~follow:FU.Skip FU.Is_link dir accumulate [] in
    let cmp (_, (_, _, kind_a) as a) (_, (_, _, kind_b) as b) =
      match kind_a, kind_b with
      | `Directory, `Directory -> compare a b
      | `Directory, _ -> -1
      | _, _ -> compare a b
    in
    List.map (fun (s, (t, e, k)) -> s, Symlink (t, e, k)) (List.sort cmp links)

  let build ~pkg_size settings =
    let dir = settings.directory.basename in
    let script = script ~script:settings.script ~pkg_size in
    let meta, install_actions, uninstall_actions = script in
    let expand_id = sp "expand_%s" dir in
    let symlinks_install_actions = segregate_symlinks settings.directory.path in
    let install_actions =
      (* we want to expand the content of dir so we suffix it with '/' *)
      (expand_id, Expand (dir ^ "/", "."))
      :: run_install_scripts settings.install_scripts
      @ symlinks_install_actions
      @ install_actions
    in
    let uninstall_actions =
      (if symlinks_install_actions <> [] then [ Reverse "symlink" ] else [])
      @ [ Reverse expand_id ]
      @ uninstall_actions
    in
    meta, install_actions, uninstall_actions
end

let output_file meta =
  let version = string_of_version meta.version in
  match meta.target with
  | None -> sp "%s-%s-%s.txz" meta.name version meta.host
  | Some target -> sp "%s-%s-%s-%s.txz" meta.name version target meta.host

let archive settings (meta, install_actions, _) script_dir script_name =
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
    [| "-C"; script_dir |];
    [| "-s"; sp "/%s/%s/" script_name "package_script.el"; script_name |];
    install_scripts settings.install_scripts;
    [| "-C"; settings.directory.dirname; settings.directory.basename |]
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
  Pre_sexp.to_string_hum (TypesSexp.Of.script (metadata, [], []))

let generate settings =
  let pkg_size = fst (FileUtil.du [ settings.directory.path ]) in
  let script = Package_script.build ~pkg_size settings in
  let script_sexp = Pre_sexp.to_string_hum (TypesSexp.Of.script script) in
  let script_path, oc = Filename.open_temp_file "package_script-" ".el" in
  output_string oc script_sexp;
  close_out oc;
  let output_file = archive settings script (Filename.dirname script_path) (Filename.basename script_path) in
  Unix.unlink script_path;
  Printf.printf "Package created as: %s\n." output_file

let main opts =
  let init =
    { output = ""; script = ""; install_scripts = None;
      directory = dir_of_path ""; template = false } in
  let l = [
    "--output", (fun ~accu n o ->
      { accu with output = Args.Get.string n o });
    "--script", (fun ~accu n o ->
      { accu with script = Args.Get.string n o });
    "--iscripts", (fun ~accu n o ->
      { accu with install_scripts = Some (dir_of_path (Args.Get.string n o)) });
    "--directory", (fun ~accu n o ->
      { accu with directory = dir_of_path (Args.Get.string n o) });
    "--template", (fun ~accu n o ->
      { accu with template = Args.Get.bool n o });
  ]
  in
  let opts = Args.fold_values ~where:"--makepkg" ~init l opts in
  if opts.template then
    print_endline (dummy_script ())
  else
    if (opts.output = "" || opts.script = "" || opts.directory.path = "") then
      raise (Args.Parsing_failed "All of --output, --script and --directory are mandatory.")
    else
      generate opts

let cli_spec =
  let mk ~n ~h c = Args.spec ~name:n ~help:h ~children:c in
  (* let usage_msg = "\
Use either (-o, -script, -iscripts and a directory) XOR -template (see -help).
Examples:
  $ makeypkg -o /some/dir -script pcre.META -iscripts iscripts-atk package-atk
  $ makeypkg -template"
  in *)
  mk ~n:"--makepkg" ~h:"Create a yypkg package from a directory." [
    mk ~n:"--output" ~h:"output directory (defaults to current dir)" [];
    mk ~n:"--script" ~h:"package script file (- for stdin)" [];
    mk ~n:"--iscripts" ~h:"directory of install scripts" [];
    mk ~n:"--directory" ~h:"directory to package" [];
    mk ~n:"--template" ~h:"write a template script on stdout" [];
  ];
