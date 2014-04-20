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
open Types.Repo

let filename_check_suffix ext s =
  try FilePath.check_extension s ext with FilePath.NoExtension _ -> false

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
    let files = Lib.Archive.list (Lib.Archive.Filename file) in
    let pkg = {
      metadata;
      size_compressed = (FileUtil.stat file).FileUtil.size;
      filename = (FilePath.basename file);
      signature = None;
      files;
      deps = [];
      sha3 = Lib.sha3_file file;
    } in
    memoizer#add file pkg;
    pkg

let repository_metadata pkglist =
  let hosts =
    let triplets = List.rev_map (fun p -> p.metadata.host) pkglist in
    Lib.rev_uniq (List.sort compare triplets)
  in
  let targets =
    let triplets = List.rev_map (fun p -> p.metadata.target) pkglist in
    Lib.rev_uniq (List.sort compare (Lib.rev_may_value triplets))
  in
  match targets, hosts with
  | [ target ], [ host ] -> { target; host; pkglist }
  | [], [ host ] -> { target = host; host; pkglist }
  | [], [] ->
      Lib.ep "Error: no target and no host found";
      assert false
  | _, _ ->
      (if targets <> [] then
        Lib.ep "Error: several targets seen: %s\n" (String.concat ", " targets));
      (if hosts <> [] then
        Lib.ep "Error: several hosts seen: %s\n" (String.concat ", " hosts));
      assert false

module Output = struct
  module HTML = struct
    let td (a, s) =
      Lib.sp "<td align=%S>%s</td>" (match a with `Right -> "right" | _ -> "") s
    let tr l =
      String.concat " " [ "<tr>"; String.concat " " (List.map td l); "</tr>" ]
    let tr_header =
      tr [
        `Left, "Name";
        `Right, "Version";
        `Right, "Size compressed";
        `Right, "Size expanded";
        `Left, "Host";
        `Left, "Target";
        `Left, "Constraints";
        `Left, "Description";
        `Left, "Dependencies";
      ]
    let tr_pkg { deps; size_compressed; metadata = m } =
      let of_size = FileUtil.string_of_size ~fuzzy:true in
      let sp_predicate (k, v) = String.concat "=" [ k; v ] in
      tr [
        `Left, m.name;
        `Right, string_of_version m.version;
        `Right, of_size size_compressed;
        `Right, of_size m.size_expanded;
        `Left, m.host;
        `Left, (match m.target with Some target -> target | None -> "N/A");
        `Left, (String.concat ", " (List.map sp_predicate m.predicates));
        `Left, m.description;
        `Left, String.concat ", " deps
      ]

    let table pkgs =
      String.concat "\n" (List.concat [
        [ "<table border=\"1\">" ];
        [ tr_header ];
        List.sort compare (List.rev_map tr_pkg pkgs);
        [ "</table>" ]
      ])
  end

  let package_list ~directory ~repository =
    let el = "package_list.el" in
    let file = FilePath.concat directory el in
    let oc = open_out_bin file in
    Pre_sexp.output_hum oc (TypesSexp.Of.repository repository);
    close_out oc;
    let xz_opt = Yylib.xz_opt Unix.LargeFile.((stat file).st_size) in
    let tar_args = [ [| "-C"; directory; el |] ] in
    Yylib.tar_xz ~tar_args ~xz_opt ~out:(file ^ ".tar.xz")

  let html ~directory ~repository =
    let html_oc = FilePath.concat directory "package_list.html" in
    let html_oc = open_out_bin html_oc in
    output_string html_oc (HTML.table repository.pkglist);
    close_out html_oc

  let write ~directory ~repository =
    package_list ~directory ~repository;
    html ~directory ~repository
end

let generate dir =
  let memoizer_pkgs = new memoizer ~directory:dir ~name:"pkg" in
  let files = Array.to_list (Sys.readdir dir) in
  let files = List.filter (filename_check_suffix "txz") files in
  (* Build the list without deps first. *)
  let pkgs = ListLabels.rev_map files ~f:(fun f ->
    pkg_of_file ~memoizer:memoizer_pkgs (FilePath.concat dir f)) in
  (* Add deps during a second stage. *)
  let pkg_compare a b = compare a.metadata.name b.metadata.name in
  let repository = repository_metadata (List.sort pkg_compare pkgs) in
  Output.write ~directory:dir ~repository;
  memoizer_pkgs#commit ()

type repository_opts = {
  generate : string;
}

let main opts =
  let l = [
    "--generate", (fun ~accu n o ->
      { accu with generate = Args.Get.string n o })
  ]
  in
  let init = { generate = "" } in
  let opts = Args.fold_values ~where:"--repository" ~init l opts in
  generate opts.generate

let cli_spec =
  let mk ~n ~h c = Args.spec ~name:n ~help:h ~children:c in
  mk ~n:"--repository" ~h:"operations related to managing whole repositories" [
    mk ~n:"--generate" ~h:"generate repository data" [];
  ];
