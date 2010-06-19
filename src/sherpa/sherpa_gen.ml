open Types

let decompress_list tar_kind input =
  let compressor = Lib.compressor_of_ext input in
  let c = [| compressor; "-d"; "-c"; input |] in
  assert (not ("Win32" = Sys.os_type));
  let maybe_wildcards = if BSD = tar_kind
    then [| |]
    else [| "--wildcards" |]
  in
  let t = Array.append [| Lib.tar; "tf"; "-" |] maybe_wildcards in
  let c_out, c_in = Unix.pipe () in
  let t_out, t_in = Unix.pipe () in
  let pid_c = Unix.create_process c.(0) c Unix.stdin c_in Unix.stderr in
  let pid_t = if BSD = tar_kind
    then Unix.create_process t.(0) t c_out Unix.stdout t_in
    else Unix.create_process t.(0) t c_out t_in Unix.stderr
  in
  let l = Lib.read pid_t t_out in
  ignore (Unix.waitpid [ Unix.WNOHANG ] pid_c);
  List.iter Unix.close [ c_out; c_in; t_out; t_in ];
  l

let tar_grep filelist expr ext file =
  let tar_args = [| "-O"; "--wildcards"; "*." ^ ext |] in
  let ext_re = Str.regexp (Printf.sprintf ".*%s$" ext) in
  let re = Str.regexp expr in
  if List.exists (fun x -> Str.string_match ext_re x 0) filelist then
    let l = Lib.decompress_untar GNU tar_args file in
    let l = List.filter (fun x -> Str.string_match re x 0) l in
    let l = List.rev_map (Str.replace_first re "") l in
    List.concat (List.rev_map (Str.split (Str.regexp " ")) l)
  else
    []

let pkg_of_file folder file = 
  let file_absolute = Filename.concat folder file in
  let metadata, _, _ = Lib.open_package Lib.tar_kind file_absolute in
  let filelist = decompress_list Lib.tar_kind file_absolute in
  let pc_requires = tar_grep filelist "Requires:" "pc" file_absolute in
  let la_requires = tar_grep filelist "dependency_libs=" "la" file_absolute in
  List.iter print_endline pc_requires;
  List.iter print_endline la_requires;
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
