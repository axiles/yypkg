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

  let add_row ~row ~w ~t ~i =
    let columns, on_select, row_gen = row in

    let bg = Elm_bg.addx w in
    let bg_reset = if i mod 2 = 0 then t.bg_reset_even else t.bg_reset_odd in
    bg_reset bg;
    Elm_table.pack t.table bg 0 i columns 1;

    let pack j o = Elm_table.pack t.table o j i 1 1 in
    row_gen pack;

    let click_zone = click_zone w ~on_mouse_down:(fun _e _o md ->
      if md.Evas_event.Mouse_down.button = 1 then (
        Queue.iter (fun (bg_reset, bg) -> bg_reset bg) t.q_selected;
        Queue.clear t.q_selected;
        Evas_object.color_set bg 0 0 120 120;
        Queue.push (bg_reset, bg) t.q_selected;
        on_select ()
      )
    )
    in
    Elm_table.pack t.table click_zone 0 i columns 1

  let rec add_rows ~populate ~w ~t i =
    let rec f i =
      match populate () with
      | Some row -> add_row ~row ~w ~t ~i; f (i+1)
      | None -> i
    in
    f 0

  let table ~scroller ~populate w =
    Elm_scroller.content_min_limit scroller 1 0;
    Elm_scroller.propagate_events_set scroller true;
    let table = Elm_table.addx w in
    Elm_table.padding_set table 2 0;
    Elm_object.content_set scroller table;
    let bg_reset_even bg = Evas_object.color_set bg 60 60 60 120 in
    let bg_reset_odd bg = Evas_object.color_set bg 0 0 0 0 in
    let q_selected = Queue.create () in
    let t = { table; bg_reset_even; bg_reset_odd; q_selected } in
    ignore (add_rows ~populate ~w ~t 0);
    t
end
