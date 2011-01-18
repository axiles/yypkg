open Printf
open Types

let la : (string, string) Hashtbl.t = Hashtbl.create 20
let pc : (string, string) Hashtbl.t = Hashtbl.create 20
let libs : (string, string) Hashtbl.t = Hashtbl.create 20

(* Update the hashtables to mention with the names of the packages containing
 * the various files *)
let update_list l h id =
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

let filename_check_suffix ext s =
  Filename.check_suffix s ext

let tar_grep filelist expr ext file =
  (* Str.global_replace quotes_re "\\2" will strip outer quotes from a string *)
  let quotes_re = Str.regexp "\\(\"\\|'\\)\\(.*\\)\\(\"\\|'\\)" in
  (* Str.split split_re will split comma-or-space-separated words *)
  let split_re = Str.regexp "\\( \\|,\\)" in
  let re = Str.regexp expr in
  (* We first check if at least a file with the right extension can be found in
   * the archive because otherwise, tar will exit with a non-zero status and
   * we'd have to manage that failure. *)
  if List.exists (filename_check_suffix ext) filelist then
    (* Get the contents of all "*.ext" files in the package *)
    let l = Lib.from_tar (`get ("*." ^ ext)) file in
    (* Only keep the lines that match expr: "Requires" ones for pkgconfig eg *)
    let l = List.filter (fun x -> Str.string_match re x 0) l in
    (* But we want the "value" in these lines, not the "key" part *)
    let l = List.rev_map (Str.replace_first re "") l in
    (* The value might be surrounded with quotes: strip these *)
    let l = List.rev_map (Str.global_replace quotes_re "\\2") l in
    (* The value is itself a list of other comma-or-space-separated values *)
    List.concat (List.rev_map (Str.split split_re) l)
  else
    []
    
(* We want to filter the "version" part of a .pc's "Requires" field, see:
  * libgphoto2.pc:Requires: libgphoto2_port >= 0.6.2, libexif >= 0.6.13 *)
let pc_split l = 
  let pred s =
    s <> "" && match s.[0] with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
  List.filter pred l

(* Check that a library is provided by one and only one package *)
let x_provides name filelist ext h =
  (* Get a shortname from a filename *)
  let f s = 
    Filename.basename (Filename.chop_extension s)
  in
  let provides = List.find_all (filename_check_suffix ext) filelist in
  let provides = List.rev_map f provides in
  update_list provides h name

(* create Types.pkg record given a yypkg package. Without listing deps *)
let pkg_of_file folder file = 
  let file_absolute = Filename.concat folder file in
  let metadata, _, _ = Lib.open_package file_absolute in
  let filelist = Lib.from_tar `list file_absolute in
  (* When a .pc file is found, add it to the global list of .pc files.
   * Same for .la, .dll, .so, .a, ... files *)
  let rels = [ "pc", pc; "la", la; "dll", libs; "so", libs; "a", libs ] in
  List.iter (fun (a, b) -> x_provides metadata.name filelist a b) rels;
  {
    metadata = metadata;
    size_compressed = (FileUtil.stat file_absolute).FileUtil.size;
    filename = file;
    signature = None;
    files = filelist;
    deps = [];
  }

(* Best effort to get back the filename corresponding to a value in a .la
 * dependency_libs field *)
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

(* Take Types.pkg record and fill the "deps" field.
 * For that, we need the list of packages so we can see which one provides the
 * files we require. *)
let add_deps packages folder pkg = 
  let list_deps h name deplst =
    let deplst = List.find_all (fun s -> s <> name && Hashtbl.mem h s) deplst in
    List.rev_map (Hashtbl.find h) deplst
  in
  let file_absolute = Filename.concat folder pkg.filename in
  (* Get the list of library a package depends on as written in its .la and .pc
   * files *)
  let pc_requires = tar_grep pkg.files "Requires:" "pc" file_absolute in
  let la_requires = tar_grep pkg.files "dependency_libs=" "la" file_absolute in
  (* Filter the "version" part of a .pc's "Requires" field, see:
    * libgphoto2.pc:Requires: libgphoto2_port >= 0.6.2, libexif >= 0.6.13 *)
  let pc_requires = pc_split pc_requires in
  (* Get the library names from the content of the dependency_libs field in .la
   * files: some subvalues aren't lib names and some forms are unhandled so skip
   * them, at least for now. *)
  let la_requires = Lib.list_rev_map_exn filename_of_libtool la_requires in
  (* We find packages that provide the needed .pc, .la or .so/.a files *)
  let pc_requires = list_deps pc pkg.metadata.name pc_requires in
  let la_requires = list_deps la pkg.metadata.name la_requires in
  let la_libs_requires = list_deps libs pkg.metadata.name la_requires in
  (* Found the deps: concat them, sort them and remove duplicates. *)
  let requires = List.concat [ pc_requires; la_requires; la_libs_requires ] in
  let requires = Lib.rev_uniq (List.fast_sort compare requires) in
  { pkg with deps = requires }

let () =
  let folder = Sys.argv.(1) in
  let files = Array.to_list (Sys.readdir folder) in
  let files = List.filter (filename_check_suffix "txz") files in
  let files = List.fast_sort compare files in
  (* Build the list without deps first. *)
  let l = List.rev_map (pkg_of_file folder) files in
  (* Add deps during a second stage. *)
  let l = List.rev_map (add_deps l folder) l in
  let l = Sexplib.Sexp.to_string_hum (sexp_of_pkglist l) in
  print_string l
