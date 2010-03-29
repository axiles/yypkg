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
}

let strip_trailing_slash s =
  (* dir_sep's length is 1 *)
  if s.[String.length s - 1] = dir_sep.[0] then
    String.sub s 0 (String.length s - 1)
  else
    s

let parse_command_line () = 
  let output,folder,pkg_name,version,packager_email,packager_name,description,arch =
    ref "", ref "", ref "", ref "", ref "", ref "", ref "", ref ""
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
    "-arch", Arg.Set_string arch, "arch";
  ]
  in
  let usage_msg = "All arguments mentionned in --help are mandatory." in
  (* the last argument is the folder to package *)
  let () = Arg.parse lst ((:=) folder) usage_msg in
  (* check if any argument has not been set (missing from the command-line *)
  if List.exists ((=) "") [!output; !folder; !pkg_name; !version; !packager_name; !packager_email; !description] then
    let () = prerr_endline usage_msg in
    exit 0
  else
    let folder = strip_trailing_slash !folder in
    let dirname = FilePath.DefaultPath.dirname (
      if not (FilePath.DefaultPath.is_relative folder) then folder
      else FilePath.DefaultPath.make_absolute install_path folder )
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
    }

let meta ~cmd_line ~pkg_size =
  String.concat "\n" [
    sprintf "(description \"%s\")" cmd_line.descr;
    sprintf "(package_name \"%s\")" cmd_line.pkg_name;
    sprintf "(package_version \"%s\")" (string_of_version cmd_line.version);
    sprintf "(package_size_expanded \"%s\")" pkg_size;
    sprintf "(packager_name \"%s\")" cmd_line.pkger_name;
    sprintf "(packager_email \"%s\")" cmd_line.pkger_email;
  ]

let package_script_el cmd_line ~pkg_size =
  let folder = cmd_line.folder_basename in
  let meta = meta ~cmd_line ~pkg_size in
  let install= sprintf "(\"%s\" (Expand \"%s/*\" \"%s\"))" folder folder "." in
  let uninstall = sprintf "(Reverse \"%s\")" folder in
  sprintf "(\n(\n%s\n)\n(\n%s\n)\n(\n%s\n)\n)" meta install uninstall

let write_temp_file base_name contents =
  let path = Filename.concat Filename.temp_dir_name base_name in
  let oc = open_out_bin path in
  let () = output_string oc contents in
  let () = close_out oc in
  path

let () =
  let cmd_line = parse_command_line () in
  let compressor = try compressor_of_ext cmd_line.output with 
    | _ -> raise Package_name_must_end_in_txz_tgz_or_tbz2
  in
  let pkg_size= FileUtil.string_of_size (fst (FileUtil.du [cmd_line.folder])) in
  let package_script_el = package_script_el ~pkg_size cmd_line in
  let script_path = write_temp_file "package_script.el" package_script_el in
  let script_path_dirname = FilePath.DefaultPath.dirname script_path in
  let script_path_basename = FilePath.DefaultPath.basename script_path in
  let tar_args = [| "-C"; script_path_dirname; script_path_basename; "-C"; cmd_line.folder_dirname; cmd_line.folder_basename |]
  in
  (* FIXME: this won't work on windows as compressor will be an absolute path *)
  (* maybe check if basename(compressor) starts with "xz" *)
  let snd = if compressor = "xz" then
    [| compressor; "--x86"; "--lzma2=dict=67108864,lc=3,lp=0,pb=2,mode=normal,nice=64,mf=bt4,depth=0" |]
  else [| compressor; "-9" |]
  in
  tar_compress tar_args snd cmd_line.output
