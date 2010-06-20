open Types

let la : (string, string) Hashtbl.t = Hashtbl.create 20
let pc : (string, string) Hashtbl.t = Hashtbl.create 20
let libs : (string, string) Hashtbl.t = Hashtbl.create 20

let merge_list l h id =
  List.iter (fun x ->
    if Hashtbl.mem h x then
      let e = Hashtbl.find h x in
      let () = Printf.eprintf "%s can't provide %s: %s already does." id x e in
      assert false
    else
      Hashtbl.add h x id
  ) l

let check_suffix ext s =
  Filename.check_suffix s ext

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
  let quotes_re = Str.regexp "\\(\"\\|'\\)\\(.*\\)\\(\"\\|'\\)" in
  let re = Str.regexp expr in
  if List.exists (check_suffix ext) filelist then
    let l = Lib.decompress_untar GNU tar_args file in
    let l = List.filter (fun x -> Str.string_match re x 0) l in
    let l = List.rev_map (Str.replace_first re "") l in
    let l = List.rev_map (Str.global_replace quotes_re "\\2") l in
    List.concat (List.rev_map (Str.split (Str.regexp "\\( \\|,\\)")) l)
  else
    []
    
let pc_split l = 
  let pred s =
    s <> "" && match s.[0] with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
  List.filter pred l

let x_provides package_name filelist ext h =
  let f s = 
    Filename.basename (Filename.chop_extension s)
  in
  let provides = List.find_all (check_suffix ext) filelist in
  let provides = List.rev_map f provides in
  merge_list provides h package_name

let pkg_of_file folder file = 
  let file_absolute = Filename.concat folder file in
  let metadata, _, _ = Lib.open_package Lib.tar_kind file_absolute in
  let filelist = decompress_list Lib.tar_kind file_absolute in
  let rels = [ "pc", pc; "la", la; "dll", libs; "so", libs ] in
  List.iter (fun (a, b) -> x_provides metadata.package_name filelist a b) rels;
  {
    metadata = metadata;
    filename = file;
    signature = None;
    files = filelist;
    deps = [];
  }

let rev_uniq l =
  let rec rev_uniq_rc accu cur = function
    | t :: q when t = cur -> rev_uniq_rc accu cur q
    | t :: q -> rev_uniq_rc (t :: accu) t q
    | [] -> accu
  in
  match l with
    | t :: q -> rev_uniq_rc [ t ] t q
    | [] -> []

let filename_of_libtool s =
  if s.[0] = '-' && s.[1] = 'l' then
    String.sub s 2 (String.length s - 2)
  else
    Filename.basename (Filename.chop_extension s) 

let add_deps packages folder pkg = 
  let list_deps h package_name l =
    let l = List.find_all (fun s -> s <> package_name && Hashtbl.mem h s) l in
    List.rev_map (Hashtbl.find h) l
  in
  print_endline pkg.metadata.package_name;
  let file_absolute = Filename.concat folder pkg.filename in
  let pc_requires = tar_grep pkg.files "Requires:" "pc" file_absolute in
  let pc_requires = pc_split pc_requires in
  let pc_requires = list_deps pc pkg.metadata.package_name pc_requires in
  let la_requires = tar_grep pkg.files "dependency_libs=" "la" file_absolute in
  let la_requires = List.rev_map filename_of_libtool la_requires in
  let la_requires = list_deps la pkg.metadata.package_name la_requires in
  let requires = List.concat [ pc_requires; la_requires ] in
  let requires = List.fast_sort compare requires in
  let requires = rev_uniq requires in
  { pkg with deps = requires }

let () =
  let folder = Sys.argv.(1) in
  let files = Array.to_list (Sys.readdir folder) in
  let files = List.filter (fun s -> Filename.check_suffix s "txz") files in
  let files = List.fast_sort compare files in
  let l = List.rev_map (pkg_of_file folder) files in
  let l = List.rev_map (add_deps l folder) l in
  let l = Sexplib.Sexp.to_string (sexp_of_pkglist l) in
  print_endline l 
