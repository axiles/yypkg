open Types

let pc : (string, string) Hashtbl.t = Hashtbl.create 20
let libs : (string, string) Hashtbl.t = Hashtbl.create 20

let sp = Printf.sprintf
let ep = Printf.eprintf

(* Update the hashtables to mention with the names of the packages containing
 * the various files *)
let update_list l h id =
  List.iter (fun x ->
    if Hashtbl.mem h x then
      let e = Hashtbl.find h x in
      if e <> id then
        (ep "%s can't provide %s: %s already does." id x e;
        assert false)
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
  ListLabels.filter l ~f:(fun s ->
    s <> "" && match s.[0] with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)

(* Check that a library is provided by one and only one package *)
let x_provides name filelist ext h =
  (* Get a shortname from a filename *)
  let f s =
    Filename.basename (Filename.chop_extension s)
  in
  let provides = List.find_all (filename_check_suffix ext) filelist in
  let provides = List.rev_map f provides in
  update_list provides h name

class ['a] memoizer ~output ~name =
  let memo_file = FilePath.concat output name in
  object(self)
    val mutable memo : (string * 'a) list = []
    method exists file =
      let cs = Digest.file file in
      List.exists (fun (key, _value) -> key = cs) memo
    method get file =
      let cs = Digest.file file in
      List.assoc cs memo
    method add file data =
      let cs = Digest.file file in
      memo <- ((cs, data) :: memo)
    method commit () =
      let oc = open_out_bin memo_file in
      Marshal.to_channel oc memo [];
      close_out oc
    initializer
      try
        let ic = open_in_bin memo_file in
        memo <- Marshal.from_channel ic;
        close_in ic;
      with _ -> ()
  end

(* create Types.pkg record given a yypkg package. Without listing deps *)
let pkg_of_file ~memoizer file =
  if memoizer#exists file then
    memoizer#get file
  else
    let metadata = Yylib.metadata_of_script (Lib.open_package file) in
    let filelist = Lib.from_tar `list file in
    (* When a .pc file is found, add it to the global list of .pc files.
     * Same for .dll, .so, .a, ... files *)
    let rels = [ "pc", pc; "dll", libs; "so", libs; "a", libs ] in
    List.iter (fun (a, b) -> x_provides metadata.name filelist a b) rels;
    let pkg = {
      metadata = metadata;
      size_compressed = (FileUtil.stat file).FileUtil.size;
      filename = (FilePath.basename file);
      signature = None;
      files = filelist;
      deps = [];
    } in
    memoizer#add file pkg;
    pkg

(* Take Types.pkg record and fill the "deps" field.
 * For that, we need the list of packages so we can see which one provides the
 * files we require. *)
let add_deps packages ~memoizer folder pkg =
  let list_deps h name deps =
    let deps = List.find_all (fun s -> s <> name && Hashtbl.mem h s) deps in
    List.rev_map (Hashtbl.find h) deps
  in
  let file_absolute = Filename.concat folder pkg.filename in
  (* List libraries a package depends on as written in its .pc files *)
  let pc_requires =
    if memoizer#exists file_absolute then
      memoizer#get file_absolute
    else
      let pc_requires = tar_grep pkg.files "Requires:" "pc" file_absolute in
      memoizer#add file_absolute pc_requires;
      pc_requires
  in
  (* Filter the "version" part of a .pc's "Requires" field, see:
    * libgphoto2.pc:Requires: libgphoto2_port >= 0.6.2, libexif >= 0.6.13 *)
  let pc_requires = pc_split pc_requires in
  (* We find packages that provide the needed .pc files *)
  let pc_requires = list_deps pc pkg.metadata.name pc_requires in
  (* Found the deps: concat them, sort them and remove duplicates. *)
  let requires = Lib.rev_uniq (List.fast_sort compare pc_requires) in
  { pkg with deps = requires }

let repo_metadata pkglist =
  let targets = List.rev_map (fun p -> p.metadata.target) pkglist in
  let targets = Lib.rev_may_value targets in
  let target = Lib.rev_uniq (List.fast_sort compare targets) in
  match target with
  | [ target ] -> { repo_target = target; pkglist = pkglist }
  | _ ->
      ep "Error: several targets seen: [ %s ]\n"
        (String.concat ", " target);
      assert false

let sprint_pkg { deps; size_compressed; metadata = m } =
  let of_size = FileUtil.string_of_size ~fuzzy:true in
  sp "%s - %s (%s -> %s); Depends: %s<br>"
  m.name (string_of_version m.version) (of_size size_compressed)
  (of_size m.size_expanded) (String.concat " - " deps)

let write_output ~output ~repo =
  FileUtil.mkdir ~parent:true (FilePath.concat output "cache");
  let el_oc = FilePath.concat output "package_list.el" in
  let el_oc = open_out_bin el_oc in
  Sexplib.Sexp.output_hum el_oc (TypesSexp.Of.repo repo);
  close_out el_oc;
  let html_oc = FilePath.concat output "package_list.html" in
  let html_oc = open_out_bin html_oc in
  List.iter (fun p -> output_string html_oc (sprint_pkg p)) repo.pkglist;
  close_out html_oc

let () =
  let folder = Sys.argv.(1) in
  let output = Sys.argv.(2) in
  FileUtil.mkdir ~parent:true output;
  let memoizer_pkgs = new memoizer ~output ~name:"pkg" in
  let memoizer_deps = new memoizer ~output ~name:"deps" in
  let files = Array.to_list (Sys.readdir folder) in
  let files = List.filter (filename_check_suffix "txz") files in
  let files = List.fast_sort compare files in
  let files = List.rev_map (FilePath.concat folder) files in
  (* Build the list without deps first. *)
  let pkgs = List.rev_map (pkg_of_file ~memoizer:memoizer_pkgs) files in
  (* Add deps during a second stage. *)
  let pkgs = List.rev_map (add_deps ~memoizer:memoizer_deps pkgs folder) pkgs in
  memoizer_pkgs#commit ();
  memoizer_deps#commit ();
  let repo = repo_metadata pkgs in
  write_output ~output ~repo
