open Types

let pkg_of_file folder file = 
  let file_absolute = Filename.concat folder file in
  let metadata, _, _ = Lib.open_package Lib.tar_kind file_absolute in
  {
    metadata = metadata;
    filename = file;
    signature = None;
    sha1 = None;
    deps = [];
  }

let () =
  let folder = Sys.argv.(1) in
  let files = Array.to_list (Sys.readdir folder) in
  let files = List.filter (fun s -> Filename.check_suffix s "txz") files in
  let files = List.fast_sort compare files in
  let l = List.rev_map (pkg_of_file folder) files in
  let l = Sexplib.Sexp.to_string (sexp_of_pkglist l) in
  print_endline l
