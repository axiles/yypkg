(*
 * yypkg - A cross-platform package manager
 * Copyright (C) 2010-2014 Adrien Nader
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Types
module ST = SherpaT

let pc : (string, string) Hashtbl.t = Hashtbl.create 20
let libs : (string, string) Hashtbl.t = Hashtbl.create 20

(* Update the hashtables to mention with the names of the packages containing
 * the various files *)
let update_list l h id =
  List.iter (fun x ->
    if Hashtbl.mem h x then
      let e = Hashtbl.find h x in
      if e <> id then
        (Lib.ep "%s can't provide %s: %s already does." id x e;
        assert false)
    else
      Hashtbl.add h x id
  ) l

let filename_check_suffix ext s =
  try FilePath.check_extension s ext with FilePath.NoExtension _ -> false

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
    let l = Lib.split_by_line (Lib.Tar.get ~from:file ("*." ^ ext)) in
    (* Only keep the lines that match expr: eg "Requires" lines for pkgconfig *)
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

class ['a] memoizer ~directory ~name =
  let memo_file = FilePath.concat directory ("memo_" ^ name) in
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
    let files = Lib.Tar.list ~from:file in
    (* When a .pc file is found, add it to the global list of .pc files.
     * Same for .dll, .so, .a, ... files *)
    let rels = [ "pc", pc; "dll", libs; "so", libs; "a", libs ] in
    List.iter (fun (a, b) -> x_provides metadata.name files a b) rels;
    let pkg = {
      metadata;
      size_compressed = (FileUtil.stat file).FileUtil.size;
      filename = (FilePath.basename file);
      signature = None;
      files;
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
  let requires = Lib.rev_uniq (List.sort compare pc_requires) in
  { pkg with deps = requires }

let repo_metadata pkglist =
  let hosts =
    let triplets = List.rev_map (fun p -> p.metadata.host) pkglist in
    Lib.rev_uniq (List.sort compare triplets)
  in
  let targets =
    let triplets = List.rev_map (fun p -> p.metadata.target) pkglist in
    Lib.rev_uniq (List.sort compare (Lib.rev_may_value triplets))
  in
  match targets, hosts with
  | [ target ], [ host ] -> { ST.target; host; pkglist }
  | [], [ host ] -> { ST.target = host; host; pkglist }
  | [], [] ->
      Lib.ep "Error: not target and no host found";
      assert false
  | _, _ ->
      (if targets <> [] then
        Lib.ep "Error: several targets seen: %s\n" (String.concat ", " targets));
      (if hosts <> [] then
        Lib.ep "Error: several hosts seen: %s\n" (String.concat ", " hosts));
      assert false

module Output = struct
  module HTML = struct
    let tds l =
      let td = function
        | `Left s -> [ "<td>"; s; "</td>" ]
        | `Right s -> [ "<td align=\"right\">"; s; "</td>" ]
      in
      String.concat " " (List.concat (List.rev (List.rev_map td l)))
    let tr l =
      String.concat " " [ "<tr>"; tds l; "</tr>" ]
    let tr_header =
      tr [
        `Left "Package name";
        `Right "Version";
        `Right "Size compressed";
        `Right "Size expanded";
        `Left "Host";
        `Left "Target";
        `Left "Constraints";
        `Left "Dependencies";
      ]
    let tr_pkg { deps; size_compressed; metadata = m } =
      let of_size = FileUtil.string_of_size ~fuzzy:true in
      let sp_predicate (k, v) = String.concat "=" [ k; v ] in
      tr [
        `Left (m.name);
        `Right (string_of_version m.version);
        `Right (of_size size_compressed);
        `Right (of_size m.size_expanded);
        `Left m.host;
        `Left (match m.target with Some target -> target | None -> "N/A");
        `Left (String.concat ", " (List.map sp_predicate m.predicates));
        `Left ((String.concat ", " deps))
      ]

    let table pkgs =
      String.concat "\n" (List.concat [
        [ "<table border=\"1\">" ];
        [ tr_header ];
        List.sort compare (List.rev_map tr_pkg pkgs);
        [ "</table>" ]
      ])
  end

  let package_list ~directory ~repo =
    let el_oc = FilePath.concat directory "package_list.el" in
    let el_oc = open_out_bin el_oc in
    Pre_sexp.output_hum el_oc (TypesSexp.Of.repo repo);
    close_out el_oc

  let html ~directory ~repo =
    let html_oc = FilePath.concat directory "package_list.html" in
    let html_oc = open_out_bin html_oc in
    output_string html_oc (HTML.table repo.ST.pkglist);
    close_out html_oc

  let write ~directory ~repo =
    package_list ~directory ~repo;
    html ~directory ~repo
end

let () =
  let dir = Sys.argv.(1) in
  let memoizer_pkgs = new memoizer ~directory:dir ~name:"pkg" in
  let memoizer_deps = new memoizer ~directory:dir ~name:"deps" in
  let files = Array.to_list (Sys.readdir dir) in
  let files = List.filter (filename_check_suffix "txz") files in
  (* Build the list without deps first. *)
  let pkgs = ListLabels.rev_map files ~f:(fun f ->
    pkg_of_file ~memoizer:memoizer_pkgs (FilePath.concat dir f)) in
  (* Add deps during a second stage. *)
  let pkgs = List.rev_map (add_deps ~memoizer:memoizer_deps pkgs dir) pkgs in
  let pkg_compare a b = compare a.metadata.name b.metadata.name in
  let repo = repo_metadata (List.sort pkg_compare pkgs) in
  Output.write ~directory:dir ~repo;
  memoizer_pkgs#commit ();
  memoizer_deps#commit ()
