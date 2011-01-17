open Types
open Lib

let read () =
  sherpa_conf_of_sexp (Disk.read Yylib.sherpa_conf_path)

let write conf =
  Disk.write Yylib.sherpa_conf_path (sexp_of_sherpa_conf conf)

let update f =
  write (f (read ()))

let pkg_uri filename =
  let conf = read () in
  String.concat "/" [ conf.mirror; conf.sherpa_version; "packages"; filename ]

let pkg_list_uri () =
  pkg_uri "pkglist"

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
    match p.target with
    (* Some target: the package is from a (cross-)toolchain *)
    | Some target ->
        (target = runfor || target = "noarch")
        && (p.host = runon || p.host = "noarch")
    (* Some target: the package is not from a toolchain, it's a lib *)
    | None ->
        (p.host = runfor || p.host = "noarch")
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

let pkglist_of_uri uri =
  pkglist_of_sexp (Sexplib.Sexp.of_string (get_uri_contents uri))

let pkglist () =
  pkglist_of_uri (pkg_list_uri ())

let get_packages ~with_deps ~output_folder ~package = 
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

let guess_arch () =
  match os_type with
  | `Unix -> run_and_read_stdout [| "config.guess" |]
  | `Windows -> "i686-w64-mingw32"
