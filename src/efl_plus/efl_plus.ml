open Efl

module Radio = struct
  let grouped ~box ~w a =
    let first = ref None in
    let b = ArrayLabels.mapi a ~f:(fun i text ->
      let radio = Elm_radio.addx ~box ~text:text w in
      Elm_radio.state_value_set radio i;
      (match !first with
      | None ->
          Elm_radio.value_set radio 0;
          first := Some radio
      | Some first ->
          Elm_radio.group_add radio first;
      );
      radio
    ) in
    fun () -> a.(Elm_radio.value_get (Elm_radio.selected_object_get b.(0)))
end

module FileEntryButton = struct
  let build ~box ~w =
    let file = ref None in
    ignore (Elm_fileselector_entry.addx ~box ~text:"Browse" w ~cb:[
      Elm.connect Elm_sig.file_chosen (fun _o s -> file := s);
    ]);
    fun () -> !file
end

module Table = struct
  type t = {
    table : Evas_object.t;
    bg_reset_even : Evas_object.t -> unit;
    bg_reset_odd : Evas_object.t -> unit;
    q_selected : ((Evas_object.t -> unit) * Evas_object.t) Queue.t;
  }

  let click_zone ~on_mouse_down w =
    let click_zone = Elm_bg.addx w in
    Evas_object.color_set click_zone 0 0 0 0;
    Evas_object.repeat_events_set click_zone true;
    Evas_object.event_callback_add_mouse_down click_zone on_mouse_down;
    click_zone

  let add_row ~column_gen ~w ~t ~i =
    let bg = Elm_bg.addx w in
    let bg_reset = if i mod 2 = 0 then t.bg_reset_even else t.bg_reset_odd in
    bg_reset bg;

    let widgets = Queue.create () in
    let rec add_columns j =
      match column_gen ~j ~w with
      | None -> j
      | Some widget ->
          Queue.push (j, widget) widgets;
          add_columns (j+1)
    in
    let columns = add_columns 0 in

    let click_zone = click_zone w ~on_mouse_down:(fun _e _o md ->
      if md.Evas_event.Mouse_down.button = 1 then (
        Queue.iter (fun (bg_reset, bg) -> bg_reset bg) t.q_selected;
        Queue.clear t.q_selected;
        Evas_object.color_set bg 0 0 120 120;
        Queue.push (bg_reset, bg) t.q_selected;
      )
    )
    in

    Elm_table.pack t.table bg 0 i columns 1;
    Queue.iter (fun (j, widget) -> Elm_table.pack t.table widget j i 1 1) widgets;
    Elm_table.pack t.table click_zone 0 i columns 1

  let table ~population w =
    let table = Elm_table.addx w in
    let bg_reset_even bg = Evas_object.color_set bg 60 0 0 60 in
    let bg_reset_odd bg = Evas_object.color_set bg 0 60 0 60 in
    let q_selected = Queue.create () in
    let t = { table; bg_reset_even; bg_reset_odd; q_selected } in
    let rec add_rows i =
      match population ~i ~w with
      | Some column_gen -> add_row ~column_gen ~w ~t ~i; add_rows (i+1)
      | None -> i
    in
    ignore (add_rows 0);
    table

let data = [|
  [| "franchement"; "obligé"; "de"; "faire"; "sale"; "='(" |];
  [| "franchementzazaz"; "obligé"; "de"; "faire"; "sale"; "='(" |];
  [| "franchement aza zaz az az "; "obligé"; "de"; "faire"; "sale"; "='(" |];
  [| "franchement"; "obligé"; "de"; "faire"; "sale"; "='(" |];
  [| "franchement"; "obligé"; "de"; "faire"; "sale"; "='(" |];
  [| "franchement"; "obligé"; "de"; "faire"; "sale"; "='(" |];
  [| "franchement"; "obligé"; "de"; "faire"; "sale"; "='(" |];
  [| "franchement"; "obligé"; "de"; "faire"; "sale"; "='(" |];
  [| "franchement"; "obligé"; "de"; "faire"; "sale"; "='(" |];
  [| "franchement"; "obligé"; "de"; "faire"; "sale"; "='(" |];
  [| "franchement"; "obligé"; "de"; "faire"; "sale"; "='(" |];
  [| "franchement"; "obligé"; "de"; "faire"; "sale"; "='(" |];
  [| "franchement"; "obligé"; "de"; "faire"; "sale"; "='(" |];
  [| "franchement"; "obligé"; "de"; "faire"; "sale"; "='(" |];
  [| "franchement"; "obligé"; "de"; "faire"; "sale"; "='(" |];
|]

  let population ~i ~w =
    if i < Array.length data then
      Some (fun ~j ~w ->
        if j = 0 then
          Some (Elm_button.addx w ~text:"click!")
        else if j < Array.length data.(i) then
          let size_hint = [ `expand; `fill; `halign 0. ] in
          Some (Elm_label.addx w ~size_hint ~text:data.(i).(j-1))
        else
          None
      )
    else
      None

  let build () =
    let w = Elm_win.addx ~title:"weee" ~autodel:true "aa" in
    Evas_object.resize w 400 200;
    let box = Elm_box.addx w in
    Elm_win.resize_object_add w box;
    let scroller_addx = Elm_object.create_addx Elm_scroller.add in
    let scroller = scroller_addx ~box w in
    Elm_scroller.propagate_events_set scroller true;
    let table = table ~population w in
    Elm_object.content_set scroller table;
    let _descr = Elm_label.addx w ~box ~size_hint:[] ~text:"lapins !" in
    Evas_object.show w
end

let f () =
  Elm.init ();
  Elm.policy_quit_set `last_window_closed;
  Table.build ();
  Elm.run ();
  Elm.shutdown ()

