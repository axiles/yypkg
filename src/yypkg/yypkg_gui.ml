open Efl

module Systems = struct
  let prompt_for_mirror () =
    let title = "Mirror required" in
    let s = ref "" in
    Elm.init ();
    Elm_toolbox.input_string
    ~title
    ~text:"Could not automatically find a mirror. Please provide one below.<br>An example is 'http://win-builds.org/1.5.0'."
    title
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
