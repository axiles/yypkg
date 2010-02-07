open Printf

type cmd_line = {
  output : string;
  folder : string;
}

let parse_command_line () = 
  let output = ref "" in
  let folder = ref "" in
  let lst = [
    "-o", Arg.Set_string output, "specify output file (mandatory)"
  ]
  in
  let set_folder s = 
    folder := s
  in
  let usage_msg = "makeypkg -o /path/to/package.txz /folder/to/package" in
  let () = Arg.parse lst set_folder usage_msg in
  if "" = !output || "" = !folder then
    let () = prerr_endline "Error: incorrect usage of makeypkg" in
    let () = prerr_endline usage_msg in
    exit 0
  else
    {
      output = !output;
      folder = !folder;
    }

let meta ~pkg_name ~pkg_size =
  String.concat "\n" [
    "((credits \"me\")";
    sprintf "(package_name \"%s\")" pkg_name;
    sprintf "(package_size_expanded \"%s\")" pkg_size;
    "(package_type Other)";
    "(package_version (Beta 0 0 0 0))";
    "(packager_email \"a@a.com\")";
    "(packager_name \"ME\"))";
  ]

let package_script_el folder_name ~pkg_size =
  let meta = meta ~pkg_name:"juju" ~pkg_size in
  let install = sprintf "(\"%s\" (Expand \"%s/*\" \"%s\"))" folder_name folder_name "." in
  let uninstall = sprintf "(Reverse \"%s\")" folder_name in
  sprintf "(\n%s\n(\n%s\n)\n(\n%s\n)\n)" meta install uninstall

let write_temp_file base_name contents =
  let path, oc = Filename.open_temp_file base_name "" in
  let () = output_string oc contents in
  let () = close_out oc in
  path

let () =
  let cmd_line = parse_command_line () in
  assert (Filename.check_suffix cmd_line.output ".tar");
  let pkg_size = FileUtil.string_of_size (fst (FileUtil.StrUtil.du [ "." ])) in
  let package_script_el = package_script_el ~pkg_size cmd_line.folder in
  let path = write_temp_file "package_script.el" package_script_el in
  let transform = sprintf "--transform=s#%s#package_script.el#" path in
  let command = sprintf
    "tar cvf %s --absolute-names --exclude \"install\" %s %s %s"
    cmd_line.output cmd_line.folder transform path
  in
  print_endline command;
  ignore (Sys.command command)
