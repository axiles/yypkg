open Types
open Efl

module Systems = struct
  let prompt_for_mirror () =
    let s = ref "" in
    Elm.init ();
    Elm_toolbox.input_string
    ~title:"Mirror required"
    "Could not automatically find a mirror. Please provide one below.<br>An example is 'http://win-builds.org/1.5.0'."
    (function Some s' -> s := s' | None -> assert false);
    Elm.run ();
    Elm.shutdown ();
    !s

  let systems_and_bits ~box w =
    ignore (Elm_label.addx w ~box
      ~text:"Select the system and architecture to install for.");
    let box_systems_and_bits = Elm_box.addx ~box w in
    let box_systems = Elm_box.addx ~box:box_systems_and_bits w in
    let box_bits = Elm_box.addx ~box:box_systems_and_bits w in
    Elm_box.horizontal_set box_systems_and_bits true;
    Elm_box.horizontal_set box false;
    Elm_box.horizontal_set box false;
    let systems = Efl_plus.Radio.grouped ~w ~box:box_systems
      [| "Native Windows"; "MSYS"; "Cygwin"; |] in
    let arch = Efl_plus.Radio.grouped ~w ~box:box_bits
      [| "i686"; "x86_64" |] in
    systems, arch

  let path ~box w =
    ignore (Elm_label.addx w ~box
      ~text:"Chose the installation path; for Cygwin/MSYS, select their installation root.");
    Elm_fileselector_entry.addx ~box ~text:"Browse" w

  let ok_cancel ~arch ~systems ~file_selector ~cb_ok ~box w =
    let box_ok_cancel = Elm_box.addx ~box w in
    Elm_box.horizontal_set box_ok_cancel true;
    Elm_box.homogeneous_set box_ok_cancel true;
    let ok = Elm_button.addx ~box:box_ok_cancel ~text:"OK" ~cb:[
      Elm.connect Elm_sig.clicked (fun _ ->
        let file = Elm_fileselector.path_get file_selector in
        match cb_ok ~system:(systems ()) ~arch:(arch ()) ~file with
        | "" -> Evas_object.del w; Elm.exit ()
        | s -> Elm_toolbox.message_box ~title:"Error" s (fun () -> ())
      )
    ] w in
    let cancel = Elm_button.addx ~box:box_ok_cancel ~text:"Cancel" ~cb:[
      Elm.connect Elm_sig.clicked (fun _ -> Evas_object.del w)
    ] w in
    ok, cancel

  let prompt ~cb_ok =
    let () = Elm.init () in
    let w = Elm_win.addx ~title:"weee" ~autodel:true "aa" in
    Elm.policy_exit_set `windows_del;
    Elm.policy_quit_set `last_window_closed;
    let box = Elm_box.addx w in
    Elm_box.horizontal_set box false;
    Elm_win.resize_object_add w box;
    let systems, arch = systems_and_bits ~box w in
    let file_selector = path ~box w in
    let _ok, _cancel = ok_cancel ~arch ~systems ~file_selector ~cb_ok ~box w in
    Evas_object.show w;
    Elm.run ()
end

module Display = struct
  let tasks = Hashtbl.create 200

  let add_task ~pkg ~labels ~w =
    let task = Elm_hoversel.addx w in
    Evas_object_smart.callback_add task Elm_sig.selected__item (fun _ it ->
      let label = Elm_object.item_part_text_get it () in
      Elm_object.part_text_set task label;
      Hashtbl.replace tasks pkg label
    );
    Elm_hoversel.horizontal_set task false;
    ListLabels.iter labels ~f:(fun (label, select) ->
      ignore (Elm_hoversel.item_add task ~label ());
      (if select then (
        Elm_object.part_text_set task label;
        Hashtbl.replace tasks pkg label
      ));
    );
    task

  let list_tasks () =
    let extract values =
      List.rev (Hashtbl.fold (fun pkg value accu ->
        if List.mem value values then pkg :: accu else accu
      ) tasks [])
    in
    extract [ "Install"; "Update" ], extract [ "Remove" ]

  let state ~pkg ~db ~w =
    let naviframe = Elm_naviframe.addx w in
    let push o = Elm_naviframe.item_simple_push naviframe o in
    let needs_update = Web.needs_update ~db pkg in
    let task = add_task ~pkg ~w ~labels:[
        "Keep as-is", db <> [] && not needs_update;
        "Install", db = [];
        "Update", needs_update;
        "Remove", false;
    ]
    in
    let downloading = Elm_progressbar.addx w in
    Elm_progressbar.horizontal_set downloading true;
    Elm_progressbar.unit_format_set downloading "%0.f%%";
    let checking = Elm_label.addx ~text:"Checking" w in
    let installing = Elm_label.addx ~text:"Installing" w in
    let task_item = push task in
    let checking_item = push checking in
    let downloading_item = push downloading in
    let installing_item = push installing in
    Elm_naviframe.item_promote task_item;
    naviframe, (function
      | `Download f ->
          Elm_naviframe.item_promote downloading_item;
          Elm_progressbar.value_set downloading f;
      | `Checking ->
          Elm_naviframe.item_promote checking_item;
      | `Installing ->
          Elm_naviframe.item_promote installing_item;
      | `Operation ->
          Elm_naviframe.item_promote task_item;
    )

  let row_of_pkg ~db ~conf ~pkg ~w =
    let limit_description s =
      if String.length s > 90 then
        String.sub s 0 90
      else
        s
    in
    let label text =
      Elm_label.addx w ~text ~size_hint:[ `expand; `fill; `halign 0. ]
    in
    let m = pkg.Repo.metadata in
    let labels = List.map label [
      m.name;
      string_of_version m.version;
      limit_description m.description;
      FileUtil.string_of_size ~fuzzy:true m.size_expanded;
    ]
    in
    let naviframe, state = state ~pkg ~db ~w in
    state, Array.of_list (
      naviframe
      :: labels
    )

  let on_select ~descr ~pkg () =
    let m = pkg.Repo.metadata in
    let pd = Lib.sp "<font_weight=bold><align=left>%s:</align></font_weight> %s" in
    let size s = FileUtil.string_of_size ~fuzzy:true s in
    Elm_object.part_text_set descr (String.concat "<br>" [
      pd "Name" m.name;
      pd "Version" (string_of_version m.version);
      pd "Size on disk" (size m.size_expanded);
      pd "Size compressed" (size pkg.Repo.size_compressed);
      pd "Description" m.description;
    ])

  let populate_table ~db ~conf ~descr ~pkglist ~w =
    let pkglist = ref pkglist in
    fun () ->
      match !pkglist with
      | [] -> None
      | pkg :: tl ->
          pkglist := tl;
          let state, row = row_of_pkg ~db ~conf ~pkg ~w in
          Some (
            Array.length row,
            on_select ~pkg ~descr,
            fun pack -> Array.iteri pack row
          )

  let pkglist = ref []

  let fetch_and_fill ~conf ~db ~table ~w ~descr =
    let inwin = Elm_inwin.add w in
    let label = Elm_label.addx ~inwin ~text:"Downloading..." inwin in
    Evas_object.show inwin;
    ignore (Thread.create (fun () ->
      pkglist := Web.packages ~conf ~follow:false ~wishes:["all"];
    ) ());
    ignore (Ecore_timer.add 0.010 (fun () ->
      if !pkglist <> [] then (
        Evas_object.del inwin;
        ignore (Efl_plus.Table.add_rows ~w ~t:table
          ~populate:(populate_table ~db ~conf ~descr ~pkglist:!pkglist ~w));
        false;
      )
      else
        true
    ))

  let main () =
    let () = Elm.init () in
    let w = Elm_win.addx ~title:"weee" ~autodel:true "aa" in
    Evas_object.resize w 768 512;
    Elm.policy_quit_set `last_window_closed;
    let box = Elm_box.addx w in
    Elm_win.resize_object_add w box;
    Elm_box.horizontal_set box false;
    let install_bt = Elm_button.addx ~size_hint:[] ~box w in
    Elm_object.part_text_set install_bt "Install";
    let panes = Elm_panes.addx ~box w in
    Elm_panes.horizontal_set panes true;
    Elm_panes.fixed_set panes true;
    Elm_panes.content_right_size_set panes 0.25;
    let table = Efl_plus.Table.table w in
    let descr = Elm_label.addx w in
    Elm_label.line_wrap_set descr `word;
    Elm_object.part_content_set panes ~p:"top" table.Efl_plus.Table.scroller;
    Elm_object.part_content_set panes ~p:"bottom" descr;
    let conf = Config.read () in
    let db = Db.read () in
    fetch_and_fill ~conf ~db ~w ~table ~descr;
    ignore (Evas_object_smart.callback_add install_bt Elm_sig.clicked (fun _ ->
      let install_list, remove_list = list_tasks () in
      List.iter (fun p -> prerr_endline p.Repo.metadata.name) install_list;
      let packages = Web.download ~conf ~dest:Yylib.default_download_path install_list in
      (if false then
        Db.update (Upgrade.upgrade ~install_new:true conf packages));
    ));
    Evas_object.show w;
    Elm.run ()
end
