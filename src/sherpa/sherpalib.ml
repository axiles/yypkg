open Types
open Lib

let read () =
  sherpa_conf_of_sexp (Disk.read Yylib.sherpa_conf_path)

let write conf =
  Disk.write Yylib.sherpa_conf_path (sexp_of_sherpa_conf conf)

let update f =
  write (f (read ()))

let pkg_list_uri () =
  let conf = read () in
  String.concat "/" [ conf.mirror; conf.sherpa_version; "pkglist" ]

let pkg_uri filename =
  let conf = read () in
  String.concat "/" [ conf.mirror; conf.sherpa_version; "packages"; filename ]

let get_uri_contents uri =
  let a = [| wget; "-O"; "-"; "-nv"; uri |] in
  let w_out, w_in = Unix.pipe () in
  (* TODO: read stderr for logs: let log_out, log_in = Unix.pipe () in *)
  let pid = Unix.create_process wget a Unix.stdin w_in Unix.stderr in
  let l = Lib.read pid w_out in
  String.concat "" l

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

let find_packages_named pkglist name_list =
  List.filter (fun p -> List.mem p.metadata.name name_list) pkglist

let get_deps pkglist packages =
  let rec add accu p =
    let name = p.metadata.name in
    let l = List.filter (fun n -> not (List.mem n accu)) (name :: p.deps) in
    let accu = List.rev_append l accu in
    List.fold_left add accu (find_packages_named pkglist l)
  in
  let names = List.fold_left add [] packages in
  find_packages_named pkglist names

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

