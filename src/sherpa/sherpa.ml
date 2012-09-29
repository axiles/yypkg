open Types

let usage_msg = ""

let x_must_be_provided spec x =
  Printf.eprintf "%s must be provided\n" x;
  Arg.usage spec usage_msg

let main () =
  let with_deps = ref false in
  let download_folder = ref Sherpalib.default_download_folder in
  let package = ref "" in
  let spec = [
    "-with-deps", Arg.Set with_deps, "get the package dependencies too";
    "-download-folder", Arg.Set_string download_folder,
      "folder to download packages to";
  ]
  in
  Arg.parse spec ((:=) package) usage_msg;
  if "" = !download_folder then
    (x_must_be_provided spec "-download-folder"; 1)
  else
    if "" = !package then
      (x_must_be_provided spec "package name"; 1)
    else
      let conf = Sherpalib.read () in
      (ignore (Sherpalib.get_packages ~conf ~with_deps:!with_deps
        ~download_folder:!download_folder ~package:!package); 0)

let () = 
  exit (main ())
