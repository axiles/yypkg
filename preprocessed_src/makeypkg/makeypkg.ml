open Printf
  
open Types
  
open Lib
  
exception Package_name_must_end_in_txz_tgz_or_tbz2
  
type cmd_line =
  { output : string; folder : string; folder_dirname : string;
    folder_basename : string; pkg_name : string; version : version;
    pkger_name : string; pkger_email : string; descr : string
  }

let strip_trailing_slash s = (* dir_sep's length is 1 *)
  if s.[(String.length s) - 1] = dir_sep.[0]
  then String.sub s 0 ((String.length s) - 1)
  else s
  
let parse_command_line () =
  let (output, folder, pkg_name, version, packager_email, packager_name,
       description) =
    ((ref ""), (ref ""), (ref ""), (ref ""), (ref ""), (ref ""), (ref "")) in
  let lst =
    [ ("-o", (Arg.Set_string output), "set output file");
      ("-name", (Arg.Set_string pkg_name), "set the package name");
      ("-version", (Arg.Set_string version), "set the package version");
      ("-email", (Arg.Set_string packager_email), "packager's email");
      ("-packager_name", (Arg.Set_string packager_name), "packager's name");
      ("-description", (Arg.Set_string description), "description") ] in
  let usage_msg = "All arguments mentionned in --help are mandatory." in
  let () = Arg.parse lst (( := ) folder) usage_msg
  in
    if
      List.exists (( = ) "")
        [ !output; !folder; !pkg_name; !version; !packager_name;
          !packager_email; !description ]
    then (let () = prerr_endline usage_msg in exit 0)
    else
      (let folder = strip_trailing_slash !folder
       in
         {
           output = !output;
           folder = folder;
           folder_dirname = FilePath.DefaultPath.dirname folder;
           folder_basename = FilePath.DefaultPath.basename folder;
           pkg_name = !pkg_name;
           version = version_of_string !version;
           pkger_name = !packager_name;
           pkger_email = !packager_email;
           descr = !description;
         })
  
let meta ~cmd_line ~pkg_size =
  String.concat "\n"
    [ sprintf "(description \"%s\")" cmd_line.descr;
      sprintf "(package_name \"%s\")" cmd_line.pkg_name;
      sprintf "(package_version \"%s\")" (string_of_version cmd_line.version);
      sprintf "(package_size_expanded \"%s\")" pkg_size;
      sprintf "(packager_name \"%s\")" cmd_line.pkger_name;
      sprintf "(packager_email \"%s\")" cmd_line.pkger_email ]
  
let package_script_el cmd_line ~pkg_size =
  let folder = cmd_line.folder_basename in
  let meta = meta ~cmd_line ~pkg_size in
  let install =
    sprintf "(\"%s\" (Expand \"%s/*\" \"%s\"))" folder folder "." in
  let uninstall = sprintf "(Reverse \"%s\")" folder
  in sprintf "(\n(\n%s\n)\n(\n%s\n)\n(\n%s\n)\n)" meta install uninstall
  
let write_temp_file base_name contents =
  let (path, oc) = Filename.open_temp_file base_name "" in
  let () = output_string oc contents in let () = close_out oc in path
  
let compressor_of_ext s =
  (* this function may raise a bunch of exceptions which should be caught with a
   * "try compressor_of_ext with _ -> ...": no need to be more specific, it only
   * means the user gave a wrong filename *)
  let ext_of_filename s =
    let l = String.length s in
    let i = String.rindex s '.' in String.sub s (i + 1) ((l - i) - 1)
  in
    match ext_of_filename s with
    | "tgz" -> gzip
    | "txz" -> xz
    | "tbz2" -> bzip2
    | _ -> assert false
  
let () =
  let cmd_line = parse_command_line () in
  let compressor =
    try compressor_of_ext cmd_line.output
    with | _ -> raise Package_name_must_end_in_txz_tgz_or_tbz2 in
  let pkg_size =
    FileUtil.string_of_size (fst (FileUtil.du [ cmd_line.folder ])) in
  let package_script_el = package_script_el ~pkg_size cmd_line in
  let script_path = write_temp_file "package_script.el" package_script_el in
  let transform =
    sprintf "--transform=s#%s#package_script.el#" script_path in
  let command =
    sprintf "%s cv --absolute-names %s -C %s %s %s | %s -9 > %s " tar
      script_path cmd_line.folder_dirname cmd_line.folder_basename transform
      compressor cmd_line.output
  in
    (print_endline command;
     let tar_args =
       [| script_path; "-C"; cmd_line.folder_dirname;
         cmd_line.folder_basename; transform
       |] in
     let snd = [| compressor; "-9" |]
     in tar_compress tar_args snd cmd_line.output)
  
