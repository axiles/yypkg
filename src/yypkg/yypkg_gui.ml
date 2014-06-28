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
  let row_of_pkg pkg w =
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
    Array.of_list (
      (Elm_button.addx w ~size_hint:[] ~text:"Install" )
      :: labels
    )

  let on_select ~descr ~pkg () =
    let m = pkg.Repo.metadata in
    let pd = Lib.sp "<font_weight=bold><align=left>%s:</align></font_weight> %s" in
    Elm_object.part_text_set descr (String.concat "<br>" [
      pd "Name" m.name;
      pd "Version" (string_of_version m.version);
      pd "Size on disk" (FileUtil.string_of_size ~fuzzy:true m.size_expanded);
      pd "Description" m.description;
    ])

  let populate_table ~descr ~pkglist ~w =
    let pkglist = ref pkglist in
    fun () ->
      match !pkglist with
      | [] -> None
      | pkg :: tl ->
          pkglist := tl;
          let row = row_of_pkg pkg w in
          Some (
            Array.length row,
            on_select ~pkg ~descr,
            fun pack -> Array.iteri pack row
          )

  let fetch_and_fill ~table ~w ~descr =
    let inwin = Elm_inwin.add w in
    let label = Elm_label.addx ~text:"Downloading..." inwin in
    Elm_inwin.content_set inwin label;
    Evas_object.show inwin;
    ignore (Thread.create (fun () ->
      let conf = Config.read () in
      let pkglist = Web.packages ~conf ~follow:false ~wishes:["all"] in
      Ecore.call (fun () ->
        Evas_object.del inwin;
        Efl_plus.Table.add_rows 0 ~w ~t:table
          ~populate:(populate_table ~descr ~pkglist ~w);
      )
    ) ())

  let main () =
    let () = Elm.init () in
    let w = Elm_win.addx ~title:"weee" ~autodel:true "aa" in
    Elm.policy_quit_set `last_window_closed;
    Evas_object.resize w 400 400;
    let box = Elm_box.addx w in
    Elm_win.resize_object_add w box;
    let scroller_addx = Elm_object.create_addx Elm_scroller.add in
    let scroller = scroller_addx ~box w in
    let descr = Elm_label.addx w ~size_hint:[ `hexpand; `fill ] in
    Elm_label.line_wrap_set descr `word;
    let table = Efl_plus.Table.table ~scroller ~populate:(populate_table ~descr ~pkglist:[] ~w) w in
    Elm_box.pack_end box descr;
    fetch_and_fill ~w ~table ~descr;
    Evas_object.show w;
    Elm.run ()
end
