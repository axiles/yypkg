open Types

let usage_msg = ""

let x_must_be_provided spec x =
  Printf.eprintf "%s must be provided\n" x;
  Arg.usage spec usage_msg

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
  if "" = !output_folder then
    (x_must_be_provided spec "-output-folder"; 1)
  else
    if "" = !package then
      (x_must_be_provided spec "package name"; 1)
    else
      let conf = Sherpalib.read () in
      (ignore (Sherpalib.get_packages ~conf ~with_deps:!with_deps
        ~output_folder:!output_folder ~package:!package); 0)

let () = 
  exit (main ())
