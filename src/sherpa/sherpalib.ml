open Types
open Lib

let read () =
  TypesSexp.To.sherpa_conf (Disk.read Yylib.sherpa_conf_path)

let write conf =
  Disk.write Yylib.sherpa_conf_path (TypesSexp.Of.sherpa_conf conf)

let update f =
  write (f (read ()))

let pkg_uri filename =
  let conf = read () in
  String.concat "/" [ conf.mirror; conf.sherpa_version; "packages"; filename ]

let repo_uri () =
  pkg_uri "repo"

let get_uri_contents uri =
  run_and_read_stdout [| wget; "-O"; "-"; "-nv"; uri |]

let get_uri uri output =
  let a = [| wget; "-O"; output; uri |] in
  let pid = Unix.create_process wget a Unix.stdin Unix.stdout Unix.stderr in
  ignore (Unix.waitpid [] pid)

let download_to_folder folder p =
  let uri = pkg_uri p.filename in
  let output = filename_concat [ folder; p.filename ] in
  FileUtil.mkdir ~parent:true ~mode:0o755 folder;
  get_uri uri output;
  output

let find_by_name pkglist name =
  List.find (fun p -> p.metadata.name = name) pkglist

let find_all_by_name pkglist name_list =
  List.filter (fun p -> List.mem p.metadata.name name_list) pkglist

let find_all_applicable ~pkglist ~runfor ~runon =
  let pred ~runfor ~runon p =
    match p.metadata.target with
    (* Some target: the package is from a (cross-)toolchain *)
    | Some target ->
        (target = runfor || target = "noarch")
        && (p.metadata.host = runon || p.metadata.host = "noarch")
    (* Some target: the package is not from a toolchain, it's a lib *)
    | None ->
        (p.metadata.host = runfor || p.metadata.host = "noarch")
  in
  List.filter (pred ~runfor ~runon) pkglist

let get_deps pkglist packages =
  let rec add accu p =
    let name = p.metadata.name in
    let l = List.filter (fun n -> not (List.mem n accu)) (name :: p.deps) in
    let accu = List.rev_append l accu in
    List.fold_left add accu (find_all_by_name pkglist l)
  in
  let names = List.fold_left add [] packages in
  find_all_by_name pkglist names

let repo_of_uri uri =
  TypesSexp.To.repo (Sexplib.Sexp.of_string (get_uri_contents uri))

let repo () =
  repo_of_uri (repo_uri ())

let pkglist () =
  let conf = read () in
  let repo = repo () in
  let runon = conf.arch in
  find_all_applicable ~pkglist:repo.pkglist ~runfor:repo.repo_target ~runon

let get_packages ~with_deps ~output_folder ~package = 
  (* NOT used in sherpa_gui so the call to repo() isn't redoing the download *)
  let pkglist = pkglist () in
  let pkglist =
    let p = List.find (fun p -> p.metadata.name = package) pkglist in
    if with_deps then get_deps pkglist [ p ] else [ p ]
  in 
  List.map (download_to_folder output_folder) pkglist

let default_output_folder =
  try
    let prefix = Unix.getenv "YYPREFIX" in
    Lib.filename_concat [ prefix; "var"; "cache"; "packages" ]
  with Not_found -> ""

