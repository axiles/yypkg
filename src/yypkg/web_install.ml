open Types
open Yylib

let get_uri_contents uri =
  Printf.eprintf "Downloading %s...%!" (Filename.basename uri);
  let content = Lib.run_and_read [| Lib.wget; "-O"; "-"; "-nv"; uri |] `stdout in
  Printf.eprintf " DONE\n%!";
  content

let get_uri uri output =
  Printf.eprintf "Downloading %s...%!" (Filename.basename uri);
  ignore (Lib.run_and_read [| Lib.wget; "-nv"; "-O"; output; uri |] `stdout);
  Printf.eprintf " DONE\n%!"

let download_to_folder ~conf folder packages =
  FileUtil.mkdir ~parent:true ~mode:0o755 folder;
  ListLabels.map packages ~f:(fun p ->
    let uri = String.concat "/" [ conf.mirror; p.filename ] in
    let output = Lib.filename_concat [ folder; p.filename ] in
    (if not (Sys.file_exists output && Lib.sha3_file output = p.sha3) then
      get_uri uri output);
    output
  )

let package_is_applicable ~conf pkg =
  let f = predicate_holds conf.predicates in
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
  TypesSexp.To.repository (Pre_sexp.of_string (get_uri_contents uri))

let repository ~conf () =
  repo_of_uri (String.concat "/" [ conf.mirror; "package_list.el"])

let pkglist ~conf  =
  let repository = repository ~conf () in
  List.filter (package_is_applicable ~conf) repository.pkglist

let get_packages ~conf ~follow ~dest ~packages =
  (* NOT used in sherpa_gui so the call to repository() isn't redoing the
   * download *)
  let pkglist =
    let repository = repository ~conf () in
    let pkglist = List.filter (package_is_applicable ~conf) repository.pkglist in
    Lib.ep "%d/%d packages available after filtering through predicates.\n"
      (List.length pkglist)
      (List.length repository.pkglist);
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
  download_to_folder ~conf dest pkglist

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
  let o = Args.fold_values ~init ~where:"--web-install" l opts in
  (* TODO: check sanity of arguments *)
  let conf = Config.read () in
  let packages = get_packages ~conf ~follow:o.follow_dependencies ~dest:o.dest ~packages:o.packages in
  (if not o.download_only then Db.update (Install.install conf packages))

let cli_spec =
  let mk ~n ~h c = Args.spec ~name:n ~help:h ~children:c in
  mk ~n:"--web-install" ~h:"download and install a package by name" [
    mk ~n:"--follow-dependencies" ~h:"also fetch and install dependencies" [];
    mk ~n:"--download-only" ~h:"only download packages, don't install them" [];
    mk ~n:"--download-folder" 
      ~h:("where to put downloaded files (instead of " ^ Yylib.default_download_path ^ ")") [];
    mk ~n:"--packages" ~h:"packages to install" [];
  ]
