open Types
open Yylib

module Get = struct
  exception Not_found
  exception Internal_Server_Error
  exception URI_parse_failure of string
  exception Hash_failure of string

  let parse_uri uri =
    let { URI.scheme; part } = Match_failure.Unsafe.t (URI.String.t' uri) in
    let fail msg = raise (URI_parse_failure msg) in
    match (scheme :> string), part with
    | "http", `Authority ({ URI.Authority.host; port; user }, path) ->
        let path = Match_failure.Unsafe.t (URI.Path.Absolute.t path) in
        `HTTP (user, host, port, path)
    | "http", _ ->
        fail (Lib.sp "http:// downloads require a website.")
    | "file", `Authority ({ URI.Authority.host = `Name []; port = None; user = None }, path) ->
        `File (URI.Path.Absolute_or_empty.String.t ~encoded:false path)
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
        let ic = open_in_bin path in
        let l = in_channel_length ic in
        try
          let b = Buffer.create l in
          Buffer.add_channel b ic l;
          close_in ic;
          Buffer.contents b
        with e ->
          close_in ic;
          raise e
    )

  let pretty_print_size ~pad s =
    let s = Int64.to_float s in
    let div s n = s /. (1024. ** (float n)) in
    if s < 1000. then
      Lib.sp "%3.0fB" s
    else
      let spec =
        if pad then format_of_string "%5.1f" else format_of_string "%3.1f" in
      let s' = div s 1 in
      if s' < 1000. then
        Lib.sp (spec ^^ (format_of_string "KB")) s'
      else
        let s' = div s 2 in
        if s' < 1000. then
          Lib.sp (spec ^^ (format_of_string "MB")) s'
        else
          Lib.sp (spec ^^ (format_of_string "GB")) (div s 3)

  let progress ~filename ~size =
    let report =
      let prefix = Printf.sprintf "GET %s" filename in
      fun ~total ~size ~speed ->
        let total = pretty_print_size ~pad:true (Int64.of_int total) in
        let speed = pretty_print_size ~pad:true (Int64.of_float speed) in
        if size <> 0L then
          Printf.printf "\r%s\t%s/%s\t%s/s%!"
            prefix
            total
            (pretty_print_size ~pad:false size)
            speed
        else
          Printf.printf "\r%s\t%s\t%s/s%!"
            prefix
            total
            speed
    in
    let t = ref (Unix.gettimeofday ()) in
    let total = ref 0 in
    let total_last = ref 0 in
    let over = ref false in
    report ~total:0 ~size ~speed:0.;
    fun ~string:_string ~offset ~length ->
      let t' = Unix.gettimeofday () in
      total := !total + offset + length;
      if t' >= !t +. 0.5 then (
        let speed = (float (!total - !total_last)) /. (t' -. !t) in
        t := t';
        total_last := !total;
        report ~total:!total ~size ~speed
      )
      else (
        let size_i = Int64.to_int size in
        if not !over
        && !t <> t'
        && (float !total >= 0.99 *. Int64.to_float size)
        && (float !total <= Int64.to_float size) then (
          let total = min (max size_i !total) size_i in
          let speed = (float (total - !total_last)) /. (t' -. !t) in
          report ~total ~size ~speed;
          over := true
        )
      )

  let to_file ~progress:(out, _done) ~agent ~file ~uri =
    let fd = Unix.(openfile file [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644) in
    (try
      let data = body ~agent ~uri ~out in
      _done ();
      let l = String.length data in
      assert (l = Unix.write fd data 0 l)
    with exn ->
      Unix.close fd; raise exn);
    Unix.close fd

  let to_file ?(retries=0) ~sha3 ~uri ~file ~agent ~progress =
    let get_with_sha3 file =
      try
        to_file ~agent ~file ~progress ~uri;
        Some (Lib.sha3_file file)
      with _ ->
        None
    in
    let rec try_until f a n =
      match n, f a with
      | _, Some sha3' when sha3' = sha3 ->
          ()
      | 0, _ ->
          Lib.ep "No valid download after %d tries. Aborting!\n%!" (retries+1);
          raise (Hash_failure file)
      | n, _ ->
          Lib.ep "Wrong file hash, trying again (%d/%d).\n%!" (n+1) (retries+1);
          try_until f a (n - 1)
    in
    try_until get_with_sha3 file retries

  let to_string ~retries ~progress ~agent ~uri =
    let rec try_until n =
      let s = try body ~agent ~uri ~out:progress with
      | _ when n = 0 ->
          print_newline ();
          Lib.ep "No valid download after %d tries. Aborting!\n%!" (retries+1);
          raise (Failure (Lib.sp "Failed download %S." uri))
      | _ ->
          print_newline ();
          Lib.ep "Download failed, trying again (%d/%d).\n%!" (n+1) (retries+1);
          try_until (n-1)
      in
      print_newline ();
      s
    in
    try_until retries
end

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

let progress_cli p =
  let size = Lib.int64_of_fileutil_size p.Repo.size_compressed in
  let version, build = p.Repo.metadata.version in
  let filename =
    match p.Repo.metadata.host, p.Repo.metadata.target with
    | host, Some target when target = host ->
        Lib.sp "%s %s-%d (%s)" p.Repo.metadata.name version build target
    | _ ->
        Lib.sp "%s %s-%d" p.Repo.metadata.name version build
  in
  Get.progress ~size ~filename, print_newline

let download_init ~conf ~dest =
  FileUtil.mkdir ~parent:true ~mode:0o755 dest;
  agent conf

let download_one ~conf ~dest ~agent ?(progress=progress_cli) p =
  let filename = p.Repo.filename in
  let sha3 = p.Repo.sha3 in
  let uri = String.concat "/" [ conf.mirror; filename ] in
  let output = Lib.filename_concat [ dest; filename ] in
  (if not (Sys.file_exists output && Lib.sha3_file output = sha3) then
    let progress = progress p in
    Get.to_file ~retries:2 ~sha3 ~agent ~file:output ~progress ~uri
  );
  output

let download ~conf ~dest ?progress packages =
  let agent = download_init ~conf ~dest in
  List.map (download_one ~conf ~dest ~agent ?progress) packages

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
  let progress = Get.progress ~size:0L ~filename:(Filename.basename uri) in
  let list_el_xz = Get.to_string ~retries:2 ~agent ~progress ~uri in
  let archive = Lib.Archive.String list_el_xz in
  let list_el = Lib.Archive.get_contents ~archive ~file:"package_list.el" in
  TypesSexp.To.repository (Pre_sexp.of_string list_el)

let repository ~conf =
  let uri = String.concat "/" [ conf.mirror; "package_list.el.tar.xz"] in
  let agent = agent conf in
  Printf.printf "Using mirror %S\n%!" conf.mirror;
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
      Printf.printf "0 package to %s.\n%!" action
  | l ->
      Printf.printf "%d packages to %s: %s\n%!"
        (List.length l)
        action
        (String.concat ", " (List.map (fun p -> p.Repo.metadata.name) l));
      Printf.printf "Press return to continue or Ctrl-C to cancel.\n%!";
      ignore (read_line ())
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
