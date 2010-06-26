open Types
open Lib

let mirror = "http://yypkg.yaxm.org"
let version = "13.1"
let pkg_list_uri = String.concat "/" [ mirror; version; "pkglist" ]

let get_uri_contents uri =
  let a = [| wget; "-O"; "-"; "-q"; uri |] in
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
  let uri = String.concat "/" [ mirror; version; "packages"; p.filename ] in
  let output = filename_concat [ folder; p.filename ] in
  FileUtil.mkdir ~parent:true ~mode:0o755 folder;
  get_uri uri output

let find_packages_named pkglist name_list =
  List.filter (fun p -> List.mem p.metadata.package_name name_list) pkglist

let rev_uniq l =
  let rec rev_uniq_rc accu cur = function
    | t :: q when t = cur -> rev_uniq_rc accu cur q
    | t :: q -> rev_uniq_rc (t :: accu) t q
    | [] -> accu
  in
  match l with
    | t :: q -> rev_uniq_rc [ t ] t q
    | [] -> []

let get_deps pkglist p =
  let rec add accu p =
    let l = List.filter (fun n -> not (List.mem n accu)) p.deps in
    let accu = List.rev_append l accu in
    List.fold_left add accu (find_packages_named pkglist l)
  in
  let names = add [ p.metadata.package_name ] p in
  find_packages_named pkglist names

let get_packages with_deps output_folder package = 
  let pkglist = get_uri_contents pkg_list_uri in
  let pkglist = pkglist_of_sexp (Sexplib.Sexp.of_string pkglist) in
  let pkglist =
    let p = List.find (fun p -> p.metadata.package_name = package) pkglist in
    if with_deps then
      get_deps pkglist p
    else
      [ p ]
  in 
  List.iter (download_to_folder output_folder) pkglist

let default_output_folder =
  try
    let prefix = Unix.getenv "YYPREFIX" in
    Lib.filename_concat [ prefix; "var"; "cache"; "packages"; version]
  with Not_found -> ""

