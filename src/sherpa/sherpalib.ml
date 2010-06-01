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
  let download_folder = filename_concat [ folder; "packages" ] in
  let uri = String.concat "/" [ mirror; version; "packages"; p.filename ] in
  let output = filename_concat [ download_folder; p.filename ] in
  FileUtil.mkdir ~parent:true ~mode:0o755 download_folder;
  get_uri uri output

let find_packages_named pkglist name_list =
  List.filter (fun p -> List.mem p.metadata.package_name name_list) pkglist

let get_deps pkglist p =
  let rec add accu p =
    let l = List.filter (fun n -> not (List.mem n accu)) p.deps in
    let accu = List.rev_append l accu in
    List.fold_left add accu (find_packages_named pkglist l)
  in
  let names = add [ p.metadata.package_name ] p in
  find_packages_named pkglist names
