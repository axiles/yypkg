open Types
open Lib

let read () =
  TypesSexp.To.sherpa_conf (Disk.read Yylib.sherpa_conf_path)

let write conf =
  Disk.write Yylib.sherpa_conf_path (TypesSexp.Of.sherpa_conf conf)

let update f =
  write (f (read ()))

let get_uri_contents uri =
  run_and_read [| wget; "-O"; "-"; "-nv"; uri |] `stdout

let get_uri uri output =
  Printf.eprintf "Downloading %s...%!" (Filename.basename uri);
  ignore (run_and_read [| wget; "-nv"; "-O"; output; uri |] `stdout);
  Printf.eprintf " DONE\n%!"

let download_to_folder ~conf folder p =
  let uri = String.concat "/" [ conf.mirror; p.filename ] in
  let output = filename_concat [ folder; p.filename ] in
  FileUtil.mkdir ~parent:true ~mode:0o755 folder;
  get_uri uri output;
  output

let find_by_name ~pkglist ~name =
  List.find (fun p -> p.metadata.name = name) pkglist

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
  TypesSexp.To.repo (Sexplib.Sexp.of_string (get_uri_contents uri))

let repo ~conf () =
  repo_of_uri (String.concat "/" [ conf.mirror; "package_list.el"])

let pkglist ~sherpa_conf ~yypkg_conf  =
  let repo = repo ~conf:sherpa_conf () in
  List.filter (package_is_applicable ~yypkg_conf) repo.pkglist

exception Unknown_package of string

let get_packages ~yypkg_conf ~sherpa_conf ~follow_deps ~dest ~packages =
  (* NOT used in sherpa_gui so the call to repo() isn't redoing the download *)
  let pkglist =
    let pkglist = pkglist ~sherpa_conf ~yypkg_conf in
    let packages =
      if packages = [ "*" ] then
        pkglist
      else
        ListLabels.rev_map packages ~f:(fun p ->
          try
            List.find (fun p' -> p = p'.metadata.name) pkglist 
          with Not_found -> raise (Unknown_package p)
        )
    in
    if follow_deps then get_deps pkglist packages else packages
  in
  List.map (download_to_folder ~conf:sherpa_conf dest) pkglist

let default_download_folder =
  try
    let prefix = Unix.getenv "YYPREFIX" in
    Lib.filename_concat [ prefix; "var"; "cache"; "packages" ]
  with Not_found -> ""

