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
open Lib

module ST = SherpaT

let read () =
  TypesSexp.To.sherpa_conf (Disk.read Yylib.sherpa_conf_path)

let write conf =
  Disk.write Yylib.sherpa_conf_path (TypesSexp.Of.sherpa_conf conf)

let update f =
  write (f (read ()))

let get_uri_contents uri =
  Printf.eprintf "Downloading %s...%!" (Filename.basename uri);
  let content = run_and_read [| wget; "-O"; "-"; "-nv"; uri |] `stdout in
  Printf.eprintf " DONE\n%!";
  content

let get_uri uri output =
  Printf.eprintf "Downloading %s...%!" (Filename.basename uri);
  ignore (run_and_read [| wget; "-nv"; "-O"; output; uri |] `stdout);
  Printf.eprintf " DONE\n%!"

let download_to_folder ~conf folder p =
  let uri = String.concat "/" [ conf.ST.mirror; p.filename ] in
  let output = filename_concat [ folder; p.filename ] in
  FileUtil.mkdir ~parent:true ~mode:0o755 folder;
  get_uri uri output;
  output

let find_all_by_name ~pkglist ~name_list =
  List.filter (fun p -> List.mem p.metadata.name name_list) pkglist

let package_is_applicable ~yypkg_conf pkg =
  let f = Config.predicate_holds yypkg_conf.preds in
  f ("host", pkg.metadata.host)
  && match pkg.metadata.target with
  | Some target -> f ("target", target)
  | None -> true

let get_deps pkglist packages =
  let rec add accu p =
    let name = p.metadata.name in
    let l = List.filter (fun n -> not (List.mem n accu)) (name :: p.deps) in
    let accu = List.rev_append l accu in
    List.fold_left add accu (find_all_by_name ~pkglist ~name_list:l)
  in
  let names = List.fold_left add [] packages in
  find_all_by_name ~pkglist ~name_list:names

let repo_of_uri uri =
  TypesSexp.To.repo (Pre_sexp.of_string (get_uri_contents uri))

let repo ~conf () =
  repo_of_uri (String.concat "/" [ conf.ST.mirror; "package_list.el"])

let pkglist ~sherpa_conf ~yypkg_conf  =
  let repo = repo ~conf:sherpa_conf () in
  List.filter (package_is_applicable ~yypkg_conf) repo.ST.pkglist

exception Unknown_package of string

let get_packages ~yypkg_conf ~sherpa_conf ~follow ~dest ~packages =
  (* NOT used in sherpa_gui so the call to repo() isn't redoing the download *)
  let pkglist =
    let repo = repo ~conf:sherpa_conf () in
    let pkglist = List.filter (package_is_applicable ~yypkg_conf) repo.ST.pkglist in
    ep "%d/%d packages available after filtering through predicates.\n"
      (List.length pkglist)
      (List.length repo.ST.pkglist);
    let packages =
      if packages = [ "all" ] then
        pkglist
      else
        ListLabels.rev_map packages ~f:(fun p ->
          try
            List.find (fun p' -> p = p'.metadata.name) pkglist 
          with Not_found -> raise (Unknown_package p)
        )
    in
    if follow then get_deps pkglist packages else packages
  in
  List.map (download_to_folder ~conf:sherpa_conf dest) pkglist

