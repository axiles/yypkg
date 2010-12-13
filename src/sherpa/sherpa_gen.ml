open Printf
open Types

let la : (string, string) Hashtbl.t = Hashtbl.create 20
let pc : (string, string) Hashtbl.t = Hashtbl.create 20
let libs : (string, string) Hashtbl.t = Hashtbl.create 20

let merge_list l h id =
  List.iter (fun x ->
    if Hashtbl.mem h x then
      let e = Hashtbl.find h x in
      if e <> id then
        let () = eprintf "%s can't provide %s: %s already does." id x e in
        assert false
      else ()
    else
      Hashtbl.add h x id
  ) l

let check_suffix ext s =
  Filename.check_suffix s ext

let tar_grep filelist expr ext file =
  let quotes_re = Str.regexp "\\(\"\\|'\\)\\(.*\\)\\(\"\\|'\\)" in
  let re = Str.regexp expr in
  if List.exists (check_suffix ext) filelist then
    let l = Lib.from_tar (`get ("*." ^ ext)) file in
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

let x_provides name filelist ext h =
  let f s = 
    Filename.basename (Filename.chop_extension s)
  in
  let provides = List.find_all (check_suffix ext) filelist in
  let provides = List.rev_map f provides in
  merge_list provides h name

let pkg_of_file folder file = 
  let file_absolute = Filename.concat folder file in
  let metadata, _, _ = Lib.open_package file_absolute in
  let filelist = Lib.from_tar `list file_absolute in
  let rels = [ "pc", pc; "la", la; "dll", libs; "so", libs; "a", libs ] in
  List.iter (fun (a, b) -> x_provides metadata.name filelist a b) rels;
  {
    metadata = metadata;
    filename = file;
    signature = None;
    files = filelist;
    deps = [];
  }

let filename_of_libtool s =
  if s.[0] = '-' then
    if s.[1] = 'l' then
      (* XXX: this will fail when..., hmmm, when... well, it's pretty brittle
       * this can fail for several reasons: if we only strip ".0" out of
       * "libfoo.so.1.2.0" or if something is appended to the library name, some
       * wildcards/regexps would probably be welcome, but when splitting the
       * filenames and adding to the hash tables, not here *)
      sprintf "lib%s" (String.sub s 2 (String.length s - 2))
    else
      if s.[1] = 'L' then (
        (* s is of the form -L/foo/bar: nothing to include, bail out *)
        Printf.eprintf "Can't get a library name from `%s'\n%!" s;
        invalid_arg "filename_of_libtool")
      else (
        Printf.eprintf "Don't know what to do with `%s'\n%!" s;
        invalid_arg "filename_of_libtool")
  else
    Filename.basename (Filename.chop_extension s) 

let add_deps packages folder pkg = 
  let list_deps h name l =
    let l = List.find_all (fun s -> s <> name && Hashtbl.mem h s) l in
    List.rev_map (Hashtbl.find h) l
  in
  let file_absolute = Filename.concat folder pkg.filename in
  let pc_requires = tar_grep pkg.files "Requires:" "pc" file_absolute in
  let pc_requires = pc_split pc_requires in
  let pc_requires = list_deps pc pkg.metadata.name pc_requires in
  let la_requires = tar_grep pkg.files "dependency_libs=" "la" file_absolute in
  let la_requires = Lib.list_rev_map_exn filename_of_libtool la_requires in
  let la_libs_requires = list_deps libs pkg.metadata.name la_requires in
  let la_requires = list_deps la pkg.metadata.name la_requires in
  let requires = List.concat [ pc_requires; la_requires; la_libs_requires ] in
  let requires = Lib.rev_uniq (List.fast_sort compare requires) in
  { pkg with deps = requires }

let () =
  let folder = Sys.argv.(1) in
  let files = Array.to_list (Sys.readdir folder) in
  let files = List.filter (fun s -> Filename.check_suffix s "txz") files in
  let files = List.fast_sort compare files in
  let l = List.rev_map (pkg_of_file folder) files in
  let l = List.rev_map (add_deps l folder) l in
  let l = Sexplib.Sexp.to_string_hum (sexp_of_pkglist l) in
  print_endline l 
