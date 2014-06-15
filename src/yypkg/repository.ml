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

let filename_check_suffix ext s =
  try FilePath.check_extension s ext with FilePath.NoExtension _ -> false

class ['a] memoizer ~directory ~name =
  let memo_file = FilePath.concat directory ("memo_" ^ name) in
  object
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
      Repo.metadata;
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
    let triplets = List.rev_map (fun p -> p.Repo.metadata.host) pkglist in
    Lib.rev_uniq (List.sort compare triplets)
  in
  let targets =
    let triplets = List.rev_map (fun p -> p.Repo.metadata.target) pkglist in
    Lib.rev_uniq (List.sort compare (Lib.rev_may_value triplets))
  in
  match targets, hosts with
  | [ target ], [ host ] -> { Repo.target; host; pkglist }
  | [], [ host ] -> { Repo.target = host; host; pkglist }
  | [], [] ->
      Lib.ep "Error: no target and no host found\n";
      assert false
  | _, _ ->
      let warn name = function
        | [] -> ()
        | l -> Lib.ep "Error: several %s seen: %s\n" name (String.concat ", " l)
      in
      warn "targets" targets;
      warn "hosts" hosts;
      assert false

let shorten_description s =
  let s = Str.(global_replace (regexp "[^(]*(\\([^)]*\\)) .*") "\\1" s) in
  Str.(global_replace (regexp "\\. .*") "" s)

module Output = struct
  module XML = struct
    let escape () v =
      let substitutes = [
        "<", "&lt;";
        ">", "&gt;";
        "<=", "&lte;";
        ">=", "&lte;";
        "&", "&amp;";
      ]
      in
      let re = Str.regexp (String.concat "\\|" (List.map fst substitutes)) in
      let replace s = List.assoc (Str.matched_string s) substitutes in
      Str.global_substitute re replace v
    let attributes { Repo.size_compressed; metadata = m } =
      let of_size = FileUtil.string_of_size ~fuzzy:true in
      let sp name value = Lib.sp "%s=\"%a\"" name escape value in
      String.concat " " [
        sp "name" m.name;
        sp "version" (string_of_version m.version);
        sp "size_compressed" (of_size size_compressed);
        sp "size_expanded" (of_size m.size_expanded);
        sp "packager_email" m.packager_email;
        sp "packager_name" m.packager_name;
        sp "host" m.host;
        sp "target" (match m.target with Some target -> target | None -> "");
      ]
    let predicate (k, v) =
      Lib.sp "<predicate key=%S value=%S/>" k v
    let comment s =
      Lib.sp "<comment>%a</comment>" escape s
    let package ({ Repo.metadata = m } as p) =
      let attributes = attributes p in
      let predicates = String.concat "" (List.map predicate m.predicates) in
      let comments = String.concat "" (List.map comment m.comments) in
      let description = shorten_description m.description in
      String.concat "\n" [
        Lib.sp "<?xml version=\"1.0\"?>";
        Lib.sp "<package %s>" attributes;
        Lib.sp "  <predicates>%s</predicates>" predicates;
        Lib.sp "  <comments>%s</comments>" comments;
        Lib.sp "  <description>%a</description>" escape description;
        Lib.sp "</package>";
      ]
    let document pkgs =
      String.concat "\n" [
        "<packages>";
        String.concat "\n" (List.sort compare (List.rev_map package pkgs));
        "</packages>";
      ]
  end
  module JSON = struct
    let predicates l =
      let predicate (k, v) =
        Lib.sp "{ \"key\" : %S, \"value\" : %S }" k v
      in
      String.concat " " [
        "[";
        String.concat ", " (List.map predicate l);
        "]"
      ]
    let comments l =
      String.concat " " [
        "[";
        String.concat ", " (List.map String.escaped l);
        "]"
      ]
    let fields { Repo.metadata = m; size_compressed } =
      let of_size = FileUtil.string_of_size ~fuzzy:true in
      let description = shorten_description m.description in
      String.concat ",\n" [
        Lib.sp "  \"name\" : %S" m.name;
        Lib.sp "  \"version\" : %S" (string_of_version m.version);
        Lib.sp "  \"size_compressed\" : %S" (of_size size_compressed);
        Lib.sp "  \"size_expanded\" : %S" (of_size m.size_expanded);
        Lib.sp "  \"packager_email\" : %S" m.packager_email;
        Lib.sp "  \"packager_name\" : %S" m.packager_name;
        Lib.sp "  \"host\" : %S" m.host;
        Lib.sp "  \"target\" : %S" (match m.target with Some target -> target | None -> "");
        Lib.sp "  \"predicates\" : %s" (predicates m.predicates);
        Lib.sp "  \"comments\" : %s" (comments m.comments);
        Lib.sp "  \"description\" : %S" description;
      ]
    let package p =
      String.concat "\n" [
        "{";
        fields p;
        "}"
      ]
    let json pkgs =
      String.concat "\n" [
        "[";
        String.concat ",\n" (List.sort compare (List.rev_map package pkgs));
        "]";
      ]
  end
  module HTML = struct
    let td (a, s) =
      Lib.sp "<td align=%S>%s</td>" (match a with `Right -> "right" | _ -> "") s
    let tr l =
      String.concat " " [ "<tr>"; String.concat " " (List.map td l); "</tr>" ]
    let tr_header =
      tr [
        `Left, "Name";
        `Right, "Version";
        `Right, "Size compressed/expanded";
        `Left, "Description";
        `Left, "Target";
        `Left, "Constraints";
        `Left, "Dependencies";
      ]
    let tr_pkg { Repo.deps; size_compressed; metadata = m } =
      let of_size = FileUtil.string_of_size ~fuzzy:true in
      let sp_predicate (k, v) = String.concat "=" [ k; v ] in
      tr [
        `Left, m.name;
        `Right, string_of_version m.version;
        `Right,
          Lib.sp "%s / %s" (of_size size_compressed) (of_size m.size_expanded);
        `Left, shorten_description m.description;
        `Left, (match m.target with Some target -> target | None -> "");
        `Left, (String.concat ", " (List.map sp_predicate m.predicates));
        `Left, String.concat ", " deps
      ]

    let table pkgs =
      String.concat "\n" (List.concat [
        (try [ Lib.sp "Host: %s<br>" (List.hd pkgs).Repo.metadata.host ]
          with _ -> []);
        [ "<table border=\"1\" cellpadding=\"3px\" style=\"border-collapse: collapse;\">" ];
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

  let write ~directory ~repository =
    let f ~extension ~serializer =
      let file = FilePath.concat directory ("package_list." ^ extension) in
      let oc = open_out_bin file in
      output_string oc (serializer repository.Repo.pkglist);
      close_out oc
    in
    package_list ~directory ~repository;
    f ~extension:"html" ~serializer:HTML.table;
    f ~extension:"xml" ~serializer:XML.document;
    f ~extension:"json" ~serializer:JSON.json
end

let pkg_compare a b =
  if a.Repo.metadata.name <> b.Repo.metadata.name then
    compare a.Repo.metadata.name b.Repo.metadata.name
  else
    compare a.Repo.metadata.version b.Repo.metadata.version

let skip_duplicates pkgs =
  let pkgs = List.sort pkg_compare pkgs in
  let pkgs = List.rev pkgs in
  let init = ("!", "!", 0), [] in
  snd (ListLabels.fold_left ~init pkgs ~f:(fun ((name, v, b) as p, pkgs) p' ->
    let name' = p'.Repo.metadata.name in
    let v', b' = p'.Repo.metadata.version in
    if name' = name then (
      Lib.ep "Skipping %s %s-%d because %s-%d is already present.\n"
         name v' b' v b;
      p, pkgs
    )
    else
      (name', v', b'), (p' :: pkgs)
  ))

let generate dir =
  let memoizer_pkgs = new memoizer ~directory:dir ~name:"pkg" in
  let files = Array.to_list (Sys.readdir dir) in
  let files = List.filter (filename_check_suffix "txz") files in
  (* Build the list without deps first. *)
  let pkgs = ListLabels.rev_map files ~f:(fun f ->
    pkg_of_file ~memoizer:memoizer_pkgs (FilePath.concat dir f)) in
  let pkgs = skip_duplicates pkgs in
  (* Add deps during a second stage. *)
  let repository = repository_metadata pkgs in
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
  ]
