open Sexplib
open Types

let get_packages with_deps output_folder package = 
  let pkglist = Sherpalib.get_uri_contents Sherpalib.pkg_list_uri in
  let pkglist = pkglist_of_sexp (Sexp.of_string pkglist) in
  let pkglist =
    let p = List.find (fun p -> p.metadata.package_name = package) pkglist in
    if with_deps then
      Sherpalib.get_deps pkglist p
    else
      [ p ]
  in 
  List.iter (Sherpalib.download_to_folder output_folder) pkglist

let default_output_folder =
  try
    let prefix = Unix.getenv "YYPREFIX" in
    Lib.filename_concat [ prefix; "var"; "cache"; "packages"; Sherpalib.version]
  with Not_found -> ""

let usage_msg = ""

let main () =
  let with_deps = ref false in
  let output_folder = ref default_output_folder in
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
      (get_packages !with_deps !output_folder !package; 0)

let () = 
  exit (main ())
