open Types
open Yylib

module Get = struct
  exception Not_found
  exception Internal_Server_Error
  exception URI_parse_failure of string

  let parse_uri uri =
    let { URI.scheme; part } = Match_failure.Unsafe.t (URI.String.t' uri) in
    let fail msg = raise (URI_parse_failure msg) in
    match (scheme :> string), part with
    | "http", `Authority ({ URI.Authority.host; port; user }, path) ->
        let path = Match_failure.Unsafe.t (URI.Path.Absolute.t path) in
        `HTTP (user, host, port, path)
    | "http", _ ->
        fail (Lib.sp "http:// downloads require a website.")
    | "file", `Absolute path ->
        `File path
    | "file", _ ->
        fail (Lib.sp "file:// downloads must be valid paths.")
    | scheme, _ ->
        fail (Lib.sp "Scheme %S is unsupported; use http:// or file://." scheme)

  let body ~agent ~uri ~out =
    match parse_uri uri with
    | `HTTP (user, host, port, path) -> (
        (try Http_get.body ~agent ~user ~host ~port ~path ~out with
        | Http_get.HTTP_.Response.Client_Error `Not_Found ->
            raise Not_found
        | Http_get.HTTP_.Response.Server_Error `Internal_Server_Error ->
            raise Internal_Server_Error
        )
      )
    | `File path -> (
        (* TODO: Not yet implemented *)
        assert false
    )

  let progress () =
    let t = ref (Unix.gettimeofday ()) in
    fun ~string:_string ~offset:_offset ~length:_length ->
      let t' = Unix.gettimeofday () in
      (if t' >= !t +. 1. then (prerr_char '.'; flush stderr; t := t'))

  let to_file ~agent ~file ~uri =
    let fd = Unix.(openfile file [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644) in
    (try
      let data = body ~agent ~uri ~out:(progress ()) in
      let l = String.length data in
      assert (l = Unix.write fd data 0 l)
    with exn ->
      Unix.close fd; raise exn);
    Unix.close fd

  let to_string ~agent uri =
    body ~agent ~uri ~out:(progress ())
end

exception Hash_failure of string

let agent conf =
  match Sys.os_type with
  | "Unix" -> "Yypkg Unix"
  | "Cygwin" -> "Yypkg Cygwin"
  | "Win32" -> (
      try
        let h = String.concat "-" (List.assoc "host_system" conf.preds) in
        Lib.sp "Yypkg Windows (%s)" h
      with Not_found -> "Yypkg Windows"
    )
  | _ -> assert false

let get_uri_contents ~agent uri =
  Printf.eprintf "Downloading %s...%!" (Filename.basename uri);
  let content = Get.to_string ~agent uri in
  Printf.eprintf " DONE\n%!";
  content

let get_uri ~agent uri output =
  Printf.eprintf "Downloading %s...%!" (Filename.basename uri);
  Get.to_file ~agent ~file:output ~uri;
  Printf.eprintf " DONE\n%!"

let download ~conf ~dest packages =
  let agent = agent conf in
  FileUtil.mkdir ~parent:true ~mode:0o755 dest;
  ListLabels.map packages ~f:(fun p ->
    let open Repo in
    let uri = String.concat "/" [ conf.mirror; p.filename ] in
    let output = Lib.filename_concat [ dest; p.filename ] in
    (if not (Sys.file_exists output && Lib.sha3_file output = p.sha3) then
      get_uri ~agent uri output;
      if Lib.sha3_file output <> p.sha3 then (
        Printf.eprintf "File downloaded but hash is wrong. Trying again.\n";
        get_uri ~agent uri output;
        if Lib.sha3_file output <> p.sha3 then (
          Printf.eprintf "File downloaded but hash is wrong AGAIN! Aborting.\n";
          raise (Hash_failure output)
        )
      )
    );
    output
  )

let package_is_applicable ~conf pkg =
  let f = predicate_holds conf.preds in
  f ("host", pkg.Repo.metadata.host)
  && match pkg.Repo.metadata.target with
  | Some target -> f ("target", target)
  | None -> true

let get_deps pkglist packages =
  let rec add accu p =
    let name = p.Repo.metadata.name in
    let l = List.filter (fun n -> not (List.mem n accu)) (name :: p.Repo.deps) in
    let accu = List.rev_append l accu in
    List.fold_left add accu (find_all_by_name ~pkglist ~name_list:l)
  in
  let names = List.fold_left add [] packages in
  find_all_by_name ~pkglist ~name_list:names

let repo_of_uri ~agent uri =
  let list_el_xz = get_uri_contents ~agent uri in
  let archive = Lib.Archive.String list_el_xz in
  let list_el = Lib.Archive.get_contents ~archive ~file:"package_list.el" in
  TypesSexp.To.repository (Pre_sexp.of_string list_el)

let repository ~conf =
  let uri = String.concat "/" [ conf.mirror; "package_list.el.tar.xz"] in
  let agent = agent conf in
  Printf.eprintf "Using mirror %S\n%!" conf.mirror;
  repo_of_uri ~agent uri

let packages ~conf ~follow ~wishes =
  let repository = repository ~conf in
  let pkglist = List.filter (package_is_applicable ~conf) repository.Repo.pkglist in
  Lib.ep "%d/%d packages available after filtering through predicates.\n"
    (List.length pkglist)
    (List.length repository.Repo.pkglist);
  if wishes = [ "all" ] || wishes = [] then
    pkglist
  else
    let l = ListLabels.rev_map wishes ~f:(fun p ->
      try
        List.find (fun p' -> p = p'.Repo.metadata.name) pkglist
      with Not_found -> raise (Unknown_package p)
    )
    in
    if follow then get_deps pkglist l else l

let needs_update ~db pkg =
  try
    let package = List.find (package_is_named pkg.Repo.metadata.name) db in
    (metadata_of_pkg package).version <> pkg.Repo.metadata.version
  with
    Not_found -> true

type web_install_opts = {
  follow_dependencies : bool;
  download_only : bool;
  dest : string;
  packages : string list;
}

let main ~start_dir opts =
  let init = { follow_dependencies = false; download_only = false;
    packages = []; dest = Yylib.default_download_path } in
  let l = [
    "--download-only", (fun ~accu n o ->
      { accu with download_only = Args.Get.bool n o });
    "--follow-dependencies", (fun ~accu n o ->
      { accu with follow_dependencies = Args.Get.bool n o });
    "--download-folder", (fun ~accu n o ->
      { accu with dest = FilePath.make_absolute start_dir (Args.Get.string n o) });
    "--packages", (fun ~accu n o ->
      { accu with packages = Args.Get.string n o :: accu.packages });
  ] in
  let o = Args.fold_values ~init ~where:"--web" l opts in
  (* TODO: check sanity of arguments *)
  let conf = Config.read () in
  let db = Db.read () in
  let l = packages ~conf ~follow:o.follow_dependencies ~wishes:o.packages in
  let need_update = List.filter (needs_update ~db) l in
  let action = if o.download_only then "download" else "update" in
  (match need_update with
  | [] ->
      Printf.eprintf "0 package to %s.\n%!" action
  | l ->
      Printf.eprintf "%d packages to %s: %s\n%!"
        (List.length l)
        action
        (String.concat ", " (List.map (fun p -> p.Repo.metadata.name) l))
  );
  let packages = download ~conf ~dest:o.dest need_update in
  if not o.download_only then
    Db.update (Upgrade.upgrade ~install_new:true conf packages)
  else
    ()

let cli_spec =
  let mk ~n ~h c = Args.spec ~name:n ~help:h ~children:c in
  mk ~n:"--web" ~h:"download and install a package by name" [
    mk ~n:"--follow-dependencies" ~h:"also fetch and install dependencies" [];
    mk ~n:"--download-only" ~h:"only download packages, don't install them" [];
    mk ~n:"--download-folder"
      ~h:("where to put downloaded files (instead of " ^ Yylib.default_download_path ^ ")") [];
    mk ~n:"--packages" ~h:"packages to install" [];
  ]
