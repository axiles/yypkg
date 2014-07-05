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
  type task =
    | Install_update
    | Keep_as_is
    | Uninstall

  let task_of_string = function
    | "Install/Update" -> Install_update
    | "Keep as-is" -> Keep_as_is
    | "Uninstall" -> Uninstall
    | _ -> assert false

  module PkgMap = Map.Make (struct
    type t = Repo.pkg
    let compare a b = compare a.Repo.metadata.name b.Repo.metadata.name
  end)

  type progress =
    | Download of float
    | Checking
    | Checked
    | Download_failed
    | Installing
    | Installed
    | Uninstalling
    | Uninstalled

  type row = (progress -> unit) * Efl_plus.Table.row

  type ui = {
    table : Efl_plus.Table.t;
    install_bt : Evas_object.t;
    descr : Evas_object.t;
    w : Evas_object.t;
    rows : row PkgMap.t;
  }

  type t = {
    db : db;
    conf : conf;
    pkglist : Repo.pkg list;
    tasks : task PkgMap.t;
    ui : ui;
  }

  type event =
    | Task of (Repo.pkg * task)
    | SetPkglist of Repo.pkg list
    | SetRows of row PkgMap.t
    | Process
    | Init

  let send = MiniFRPNot.create "event"

  let list_tasks s =
    let extract value =
      List.map fst (PkgMap.bindings (PkgMap.filter (fun _ v -> v = value) s.tasks))
    in
    extract Install_update, extract Uninstall

  let process t =
    let install_list, uninstall_list = list_tasks t in
    let get_push_state pkg =
      let push_state, _row = PkgMap.find pkg t.ui.rows in
      fun x -> (Ecore.main_loop_thread_safe_call_sync (fun () -> push_state x))
    in
    let dest = Yylib.default_download_path in
    let agent = Web.download_init ~conf:t.conf ~dest in
    let progress ~push_state pkg =
      let size = Lib.int64_of_fileutil_size pkg.Repo.size_compressed in
      let size = Int64.to_float size in
      let position = ref 0 in
      let progress_downloading ~string:_string ~offset:_offset ~length =
        let new_position = !position + length in
        let old_progress = (float !position) /. size in
        let progress = (float new_position) /. size in
        position := new_position;
        (if floor (100. *. progress) > floor (100. *. old_progress) then
          push_state (Download progress))
      in
      progress_downloading, (fun () -> Ecore.main_loop_thread_safe_call_sync (fun () -> push_state Checking))
    in
    Thread.create (fun () -> 
      let l = ListLabels.map install_list ~f:(fun pkg ->
        let push_state = get_push_state pkg in
        let progress pkg = progress ~push_state pkg in
        push_state (Download 0.);
        let file =
          try
            Web.download_one ~conf:t.conf ~dest ~agent ~progress pkg
          with e ->
            Ecore.main_loop_thread_safe_call_sync (fun () -> push_state Download_failed);
            raise e
        in
        push_state Checked;
        file, push_state
      )
      in
      Db.update (fun db ->
        let db = ListLabels.fold_left l ~init:db ~f:(fun db (f, push_state) ->
          push_state Installing;
          let db = Upgrade.upgrade ~install_new:true t.conf [ f ] db in
          push_state Installed;
          db
        )
        in
        ListLabels.fold_left uninstall_list ~init:db ~f:(fun db pkg ->
          let push_state = get_push_state pkg in
          push_state Uninstalling;
          let db = Uninstall.uninstall [ pkg.Repo.metadata.name ] db in
          push_state Uninstalled;
          db
        )
      )
    ) ()

  let add_task ~pkg ~labels ~w =
    let task = Elm_hoversel.addx w in
    Evas_object_smart.callback_add task Elm_sig.selected__item (fun _ it ->
      let label = Elm_object.item_part_text_get it () in
      Elm_object.part_text_set task label;
      !send (Task (pkg, task_of_string label))
    );
    Elm_hoversel.horizontal_set task false;
    ListLabels.iter labels ~f:(fun (label, select) ->
      ignore (Elm_hoversel.item_add task ~label ());
      (if select then (
        Elm_object.part_text_set task label;
        !send (Task (pkg, task_of_string label))
      ));
    );
    task

  let state ~pkg ~t =
    let naviframe = Elm_naviframe.addx t.ui.w in
    let push o = Elm_naviframe.item_simple_push naviframe o in
    let needs_update = Web.needs_update ~db:t.db pkg in
    let task = add_task ~pkg ~w:t.ui.w ~labels:[
      "Keep as-is", t.db <> [] && not needs_update;
      "Install/Update", t.db = [] || needs_update;
      "Uninstall", false;
    ]
    in
    let downloading = Elm_progressbar.addx t.ui.w in
    Elm_progressbar.horizontal_set downloading true;
    Elm_progressbar.unit_format_set downloading "%0.f%%";
    let checking = Elm_label.addx ~text:"Checking" t.ui.w in
    let checked = Elm_label.addx ~text:"Checked" t.ui.w in
    let download_failed = Elm_label.addx ~text:"Download failed" t.ui.w in
    let installing = Elm_label.addx ~text:"Installing" t.ui.w in
    let installed = Elm_label.addx ~text:"Installed" t.ui.w in
    let uninstalling = Elm_label.addx ~text:"Uninstalling" t.ui.w in
    let uninstalled = Elm_label.addx ~text:"Uninstalled" t.ui.w in
    let task_item = push task in
    let checking_item = push checking in
    let checked_item = push checked in
    let download_failed_item = push download_failed in
    let downloading_item = push downloading in
    let installing_item = push installing in
    let installed_item = push installed in
    let uninstalling_item = push uninstalling in
    let uninstalled_item = push uninstalled in
    Elm_naviframe.item_promote task_item;
    naviframe, (function
      | Download f ->
          Elm_naviframe.item_promote downloading_item;
          Elm_progressbar.value_set downloading f
      | Checking ->
          Elm_naviframe.item_promote checking_item
      | Checked ->
          Elm_naviframe.item_promote checked_item
      | Download_failed ->
          Elm_naviframe.item_promote download_failed_item
      | Installing ->
          Elm_naviframe.item_promote installing_item
      | Installed ->
          Elm_naviframe.item_promote installed_item
      | Uninstalling ->
          Elm_naviframe.item_promote uninstalling_item
      | Uninstalled ->
          Elm_naviframe.item_promote uninstalled_item
    )

  let row_of_pkg ~t ~pkg =
    let limit_description s =
      if String.length s > 90 then
        String.sub s 0 90
      else
        s
    in
    let label text =
      Elm_label.addx t.ui.w ~text ~size_hint:[ `expand; `fill; `halign 0. ]
    in
    let m = pkg.Repo.metadata in
    let labels = List.map label [
      m.name;
      string_of_version m.version;
      limit_description m.description;
      FileUtil.string_of_size ~fuzzy:true m.size_expanded;
    ]
    in
    let naviframe, push_state = state ~pkg ~t in
    push_state, Array.of_list (
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

  let build_rows ~t =
    ListLabels.fold_left t.pkglist ~init:PkgMap.empty ~f:(fun rows pkg ->
      let push_state, row = row_of_pkg ~t ~pkg in
      let row' = (
        Array.length row,
        on_select ~pkg ~descr:t.ui.descr,
        fun pack -> Array.iteri pack row
      )
      in
      PkgMap.add pkg (push_state, row') rows
    )

  let fetch_and_fill ~t =
    let inwin = Elm_inwin.add t.ui.w in
    let _label = Elm_label.addx ~inwin ~text:"Downloading package infos..." inwin in
    Evas_object.show inwin;
    ignore (Thread.create (fun () ->
      let pkglist = Web.packages ~conf:t.conf ~follow:false ~wishes:["all"] in
      !send (SetPkglist pkglist);
      let t = { t with pkglist } in
      Ecore.main_loop_thread_safe_call_sync (fun () ->
        Evas_object.del inwin;
        let rows = build_rows ~t in
        !send (SetRows rows);
        let t = { t with ui = { t.ui with rows } } in
        ignore (PkgMap.fold (fun _pkg (_push_state, row) i ->
          Efl_plus.Table.add_row ~row ~w:t.ui.w ~t:t.ui.table ~i;
          i + 1
        ) t.ui.rows 0);
        ignore (Evas_object_smart.callback_add
          t.ui.install_bt Elm_sig.clicked (fun _ -> !send Process));
        Elm_object.disabled_set t.ui.install_bt false;
      );
    ) ())

  let init t =
    let conf = Config.read () in
    let db = Db.read () in
    let t = { t with db; conf; pkglist = []; tasks = PkgMap.empty } in
    (* TODO: first empty the table *)
    fetch_and_fill ~t;
    t

  let state_machine t = function
    | Task (pkg, task) ->
        { t with tasks = PkgMap.add pkg task t.tasks }
    | SetPkglist pkglist ->
        { t with pkglist }
    | SetRows rows ->
        { t with ui = { t.ui with rows } }
    | Process ->
        ignore (process t);
        t
        (* init t *)
    | Init ->
        init t

  let main () =
    let () = Elm.init () in
    let w = Elm_win.addx ~title:"weee" ~autodel:true "aa" in
    Evas_object.resize w 768 512;
    Elm.policy_quit_set `last_window_closed;
    let box = Elm_box.addx ~size_hint:[] w in
    Elm_win.resize_object_add w box;
    Evas_object.size_hint_set box [ `fill; `expand ];
    Elm_box.horizontal_set box false;
    let install_bt = Elm_button.addx ~size_hint:[] ~box w in
    Elm_object.part_text_set install_bt "Install";
    Elm_object.disabled_set install_bt true;
    let panes = Elm_panes.addx ~box w in
    Elm_panes.horizontal_set panes true;
    Elm_panes.fixed_set panes true;
    Elm_panes.content_right_size_set panes 0.25;
    let table = Efl_plus.Table.table w in
    let scroller_addx = Elm_object.create_addx Elm_scroller.add in
    let scroller_descr = scroller_addx w in
    Elm_scroller.content_min_limit scroller_descr 1 0;
    let descr = Elm_label.addx w in
    Elm_label.line_wrap_set descr `word;
    Elm_object.part_content_set panes ~p:"top" table.Efl_plus.Table.scroller;
    Elm_object.part_content_set panes ~p:"bottom" scroller_descr;
    Elm_object.content_set scroller_descr descr;
    let conf = { mirror = ""; preds = [] } in
    let ui = { table; descr; install_bt; w; rows = PkgMap.empty } in
    let t = { db = [] ; conf; pkglist = []; tasks = PkgMap.empty; ui } in
    MiniFRPNot.fold state_machine t send;
    !send Init;
    Evas_object.show w;
    Elm.run ()
end
