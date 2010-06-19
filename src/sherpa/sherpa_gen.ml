open Types

let tar_grep expr ext file =
  let tar_args = [| "-O"; "--wilcards"; "*." ^ ext |] in
  let re = Str.regexp expr in
  let l = Lib.decompress_untar GNU tar_args file in
  let l = List.filter (fun x -> Str.string_match re x 0) l in
  let l = List.rev_map (Str.replace_first re "") l in
  List.concat (List.rev_map (Str.split (Str.regexp " ")) l)

let pkg_of_file folder file = 
  let file_absolute = Filename.concat folder file in
  let metadata, _, _ = Lib.open_package Lib.tar_kind file_absolute in
  let pc_requires = tar_grep "Requires:" "pc" file in
  List.iter print_endline pc_requires;
  {
    metadata = metadata;
    filename = file;
    signature = None;
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
