open Printf

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

let package_script_el f_list ~pkg_size =
  let f_list = List.filter ((<>) "install") f_list in
  let f file =
    sprintf "(\"%s\" (Expand \"%s\" \"%s\"))" file file "."
  in
  let g file =
    sprintf "(Reverse \"%s\")" file
  in
  let meta = meta ~pkg_name:"juju" ~pkg_size in
  let install = String.concat "\n" (List.map f f_list) in
  let uninstall = String.concat "\n" (List.map g f_list) in
  sprintf "(\n%s\n(\n%s\n)\n(\n%s\n)\n)" meta install uninstall

let () =
  let output = Sys.argv.(1) in
  assert (Filename.check_suffix output ".tgz");
  let f_list = Array.to_list (Sys.readdir ".") in
  let pkg_size, _ = FileUtil.StrUtil.du [ "." ] in
  let pkg_size = FileUtil.string_of_size pkg_size in
  let package_script_el = package_script_el ~pkg_size f_list in
  let oc = open_out_bin "package_script.el" in
  let () = output_string oc package_script_el in
  let () = close_out oc in
  ignore (Sys.command (sprintf "tar czf %s --exclude \"install\" ." output))
