open Types

let cmd_line_spec = [
  "-prefix", [], "prefix sherpa will be working in";
  "-follow-dependencies", [], "also fetch and install dependencies";
  "-download-folder", [],
    "download files there (instead of " ^ Yylib.default_download_path ^ ")";
  "-install", [], "install packages";
  "-download", [], "downlaod packages";
]

let settings_of_cmd_line cmd_line =
  let pred x = List.exists (fun s -> Args.is_opt ~s x) [
    "-follow-dependencies"; "-download-folder"] in
  let lt, lf = List.partition pred cmd_line in
  let follow = List.exists (Args.is_opt ~s:"-follow-dependencies") lt in
  let dest =
    try
      match List.find (Args.is_opt ~s:"-download-folder") lt with
      | Args.Opt (_, [ Args.Val dest ]) -> dest
      | Args.Opt _ -> raise (Args.Parsing_failed "Bad download folder")
      | _ -> assert false
    with Not_found -> Yylib.default_download_path
  in
  (follow, dest), lf

let main () =
  let b = Buffer.create 100 in
  if Args.wants_help () || Args.nothing_given () then
    (Args.bprint_help b cmd_line_spec; Buffer.output_buffer stderr b)
  else
    let cmd_line = Args.parse cmd_line_spec Sys.argv in
    let prefix, cmd_line = Yylib.prefix_of_cmd_line cmd_line in
    let (follow_deps, dest), cmd_line = settings_of_cmd_line cmd_line in
    let action, actionopts = Yylib.action_of_cmd_line cmd_line in
    let dest = FilePath.make_absolute (Sys.getcwd ()) dest in
    let packages = Args.to_string_list actionopts in
    Sys.chdir prefix;
    Yylib.sanity_checks ();
    let sherpa_conf = Sherpalib.read () in
    let yypkg_conf = Conf.read () in
    match action with
    | None -> ()
    | Some "-install" ->
        let packages =
          Sherpalib.get_packages ~sherpa_conf ~yypkg_conf ~follow_deps ~dest
          ~packages
        in
        Db.update (Install.install yypkg_conf packages)
    | Some "-download" ->
        ignore (Sherpalib.get_packages ~sherpa_conf ~yypkg_conf ~follow_deps
        ~dest ~packages)
    | _ -> assert false

let () = 
  main ()
