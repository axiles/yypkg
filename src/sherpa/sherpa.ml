open Types

let usage_msg = ""

let main () =
  let with_deps = ref false in
  let output_folder = ref Sherpalib.default_output_folder in
  let package = ref "" in
  let spec = [
    "-with-deps", Arg.Set with_deps, "get the package dependencies too";
    "-output-folder", Arg.Set_string output_folder, "folder to download packages to";
  ]
  in
  Arg.parse spec ((:=) package) usage_msg;
  let x_must_be_provided x =
    Printf.eprintf "%s must be provided\n" x;
    Arg.usage spec usage_msg;
    1
  in
  if "" = !output_folder then
    x_must_be_provided "-output-folder"
  else
    if "" = !package then
      x_must_be_provided "package name"
    else
      (ignore (Sherpalib.get_packages ~with_deps:!with_deps
        ~output_folder:!output_folder ~package:!package); 0)

let () = 
  exit (main ())
