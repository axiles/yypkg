open Types
open Lib
open Sherpalib

type col = {
  installed : string * bool GTree.column;
  with_deps : string * bool GTree.column;
  name : string * string GTree.column;
  version_inst : string * string GTree.column;
  version_avail : string * string GTree.column;
  size_installed : string * string GTree.column;
  size_package : string * string GTree.column;
  description : string * string GTree.column;
}

let hpolicy = `AUTOMATIC
let vpolicy = `AUTOMATIC

let get_pkglist ~conf =
  let d = GWindow.message_dialog ~message:"Sherpa is currently fetching the \
  package list. This takes a few seconds." ~title:"Fetching package list"
  ~modal:true ~deletable:false ~message_type:`INFO ~buttons:GWindow.Buttons.ok
  ~show:true () in
  (* puke, but this makes sure the dialog contents are always shown *)
  ignore (Unix.select [] [] [] 0.010);
  while Glib.Main.pending () do ignore (Glib.Main.iteration true) done;
  let pkglist = pkglist ~sherpa_conf:conf ~yypkg_conf:(Conf.read ()) in
  d#destroy ();
  pkglist

let set_dialog ~text ~callback () =
  let dlg = GWindow.dialog ~modal:true ~destroy_with_parent:true ~show:true () in
  let textfield = GEdit.entry ~width:400 ~text ~packing:dlg#vbox#pack () in
  let ok = GButton.button ~stock:`OK ~packing:dlg#action_area#add () in
  let cancel = GButton.button ~stock:`CANCEL ~packing:dlg#action_area#add () in
  ignore (ok#connect#clicked ~callback:(callback dlg textfield));
  ignore (cancel#connect#clicked ~callback:dlg#destroy)

let set_conf_field ~read ~update ~text ~f =
  let conf = read () in
  let text = text conf in
  let ok_callback dialog textfield () =
    update (f textfield#text); dialog#destroy ()
  in
  set_dialog ~text ~callback:ok_callback

let set_sherpa_conf_field prop =
  let text, f = match prop with
  | `mirror -> (fun c -> c.mirror), fun mirror c -> { c with mirror = mirror }
  | `downloadfolder -> (fun c -> c.download_folder), fun folder c -> { c with download_folder = folder }
  in
  set_conf_field ~read ~update ~text ~f

let set_yypkg_conf_field prop =
  let pred_text pred c =
    String.concat "," (try (List.assoc pred c.preds) with Not_found -> [])
  in
  let text, f = match prop with
  | `pred p -> pred_text p, fun s c -> Config.setpred c (p ^ "=" ^ s)
  in
  set_conf_field ~read:Conf.read ~update:Conf.update ~text ~f

let cols = new GTree.column_list

let columns =
  let module GD = Gobject.Data in
  let f x = cols#add x in
  {
    installed = "Selected", f GD.boolean;
    with_deps = "Deps included", f GD.boolean;
    name = "Name", f GD.string;
    version_inst = "Version (installed)", f GD.string;
    version_avail = "Version (available)", f GD.string;
    size_installed = "Size (installed)", f GD.string;
    size_package = "Size (package)", f GD.string;
    description = "Description", f GD.string;
  }
let columns_l2 =
  let x = columns in
  [ x.name; x.version_inst; x.version_avail; x.size_installed; x.size_package;
  x.description ]

let model_set ~(model:GTree.tree_store) ~iter (c, v) =
  model#set ~row:iter ~column:(snd c) v

let details_of_package pkg =
  let buffer = GText.buffer () in
  let field_tag = buffer#create_tag ~name:"field" [ `WEIGHT `BOLD ] in
  buffer#insert ~tags:[field_tag] "Description";
  buffer#insert ": ";
  buffer#insert pkg.metadata.Types.description;
  buffer

let textview ~packing =
  let scroll = GBin.scrolled_window ~packing ~hpolicy ~vpolicy () in
  GText.view ~editable:false ~packing:scroll#add_with_viewport ()

let rec partition model ~accu:(l1, l2) ~pred ~getter ~iter =
  let v = getter model iter in
  let accu =
    if pred model iter then
      (v :: l1), l2
    else
      l1, (v :: l2)
  in
  if model#iter_next iter then
    partition model ~accu ~pred ~getter ~iter
  else
    accu

let selection_changed_cb ~pkglist ~textview ~(model : GTree.tree_store) ~selection () =
  match selection#get_selected_rows with
  | path :: _ ->
      let iter = model#get_iter path in
      let name = model#get ~row:iter ~column:(snd columns.name) in
      let pkg = find_by_name pkglist name in
      let buffer = details_of_package pkg in
      textview#set_buffer buffer
  | [] -> ()

let update_listview ~(model : GTree.tree_store) ~selection_changed_cb ~selection ~pkglist db =
  let fill columns db pkg =
    let metadata = pkg.metadata in
    let name = metadata.Types.name in
    let iter = model#append () in
    ignore (selection#connect#changed ~callback:selection_changed_cb);
    let size = FileUtil.string_of_size ~fuzzy:true metadata.size_expanded in
    let size_pkg = FileUtil.string_of_size ~fuzzy:true pkg.size_compressed in
    List.iter (model_set ~model ~iter) [
      columns.name, name;
      columns.size_installed, size;
      columns.size_package, size_pkg;
      columns.description, metadata.Types.description;
    ];
    try
      let m = Yylib.metadata_of_pkg (Yylib.find_by_name db name) in
      let f c v = model_set ~model ~iter (c, v) in
      f columns.installed true;
      f columns.with_deps true;
      f columns.version_inst (string_of_version m.version)
    with _ -> ()
  in
  List.iter (fill columns db) pkglist

let selecteds_of ~model ~column =
  let is_selected (model : GTree.tree_store) iter =
    model#get ~row:iter ~column
  in
  let getter (model : GTree.tree_store) iter =
    model#get ~row:iter ~column:(snd columns.name)
  in
  match model#get_iter_first with
  | Some iter -> partition model ~accu:([], []) ~pred:is_selected ~getter ~iter
  | None -> [], []

let avail_is_newer_than_installed db p =
  try 
    let pkg = Yylib.find_by_name db p.metadata.Types.name in
    p.metadata.Types.version > (Yylib.metadata_of_pkg pkg).version
  with Not_found ->
    false

module UI = struct
  class core ~packing ~update_btn ~process_btn =
    let paned = GPack.paned ~packing `VERTICAL () in
    let textview = textview ~packing:(paned#pack2 ~shrink:true) in
    let conf = read () in
    let pkglist = get_pkglist ~conf in
    let db = Db.read () in
    object(self)
      initializer
        let tree = self#tree ~packing:(paned#pack1 ~shrink:false) in
        let selection = tree#treeview#selection in
        let selection_changed_cb = selection_changed_cb ~pkglist ~textview ~model:tree#model ~selection in
        update_listview ~model:tree#model ~selection_changed_cb ~selection ~pkglist db;
        ignore (process_btn#connect#clicked ~callback:(fun () ->
          self#process ~model:tree#model ~db));
        update_btn ~callback:(fun () ->
          tree#destroy ();
          textview#destroy ();
          paned#destroy ();
          ignore (GMain.Timeout.add ~ms:50 ~callback:(fun () ->
            ignore (new core ~process_btn ~update_btn ~packing);
            false)))

      method process ~model ~db =
        let selecteds, unselecteds = selecteds_of ~model ~column:(snd columns.with_deps) in
        let selecteds = find_all_by_name pkglist selecteds in
        let uninst = List.filter (Yylib.is_installed db) unselecteds in
        let inewer = List.filter (avail_is_newer_than_installed db) selecteds in
        let ipkgs = List.map (download_to_folder ~conf conf.download_folder) inewer in
        Db.update (Uninstall.uninstall uninst);
        Db.update (Install.install (Conf.read ()) ipkgs)

      method update_listview_deps ~(model : GTree.tree_store) =
        let rec update columns selecteds iter =
          let name = model#get ~row:iter ~column:(snd columns.name) in
          let selected = List.mem name selecteds in
          model#set ~row:iter ~column:(snd columns.with_deps) selected;
          if model#iter_next iter then update columns selecteds iter else ()
        in
        let should_be_uninstalled unselecteds db p =
          Yylib.is_installed db p.metadata.Types.name && List.mem p unselecteds
        in
        let selecteds, unselecteds = selecteds_of ~model ~column:(snd columns.installed) in
        Gaux.may (model#get_iter_first) ~f:(fun iter ->
          let selecteds = find_all_by_name pkglist selecteds in
          let unselecteds = find_all_by_name pkglist unselecteds in
          let deps = get_deps pkglist selecteds in
          let deps = List.filter (fun p -> not (should_be_uninstalled unselecteds db p)) deps in
          let deps = List.map (fun p -> p.metadata.Types.name) deps in
          update columns deps iter
        )

      method tree ~packing =
        let scrolled = GBin.scrolled_window ~packing ~hpolicy ~vpolicy () in
        let model = GTree.tree_store cols in
        let treeview = GTree.view ~model ~reorderable:true ~packing:scrolled#add_with_viewport () in
        let renderer_text = GTree.cell_renderer_text [] in
        let toggle ?f col treepath =
          let iter = model#get_iter treepath in
          model#set ~row:iter ~column:col (not (model#get ~row:iter ~column:col));
          match f with
          | None -> ()
          | Some f -> f ~model
        in
        let column_toggle ~auto ~on_toggle (title, col) =
          let renderer_toggle = GTree.cell_renderer_toggle [] in
          (if auto then
            ignore (renderer_toggle#connect#toggled ~callback:(on_toggle col))
          else
            ());
          GTree.view_column ~title ~renderer:(renderer_toggle, [ "active", col ]) ()
        in
        let column_string (title, col) =
          GTree.view_column ~title ~renderer:(renderer_text, [ "text", col ]) ()
        in
        let f = self#update_listview_deps in
        let inst = column_toggle ~auto:true ~on_toggle:(toggle ~f) columns.installed in
        let sel = column_toggle ~auto:true ~on_toggle:toggle columns.with_deps in
        let columns = inst :: sel :: List.map column_string columns_l2 in
        List.iter (fun vc -> vc#set_resizable true; vc#set_min_width 5) columns;
        ignore (List.map treeview#append_column columns);
        object
          method model = model
          method treeview = treeview
          method destroy () = treeview#destroy (); scrolled#destroy ()
        end
    end

  let menu ~packing =
    let predicates = [ `I ("Arch", set_yypkg_conf_field (`pred "arch")); ] in
    let settings = [
      `I ("Mirror", set_sherpa_conf_field `mirror);
      `I ("Download folder", set_sherpa_conf_field `downloadfolder);
      `M ("Predicates", predicates);
    ]
    in
    let menu = [
      "File", [ `I ("Quit", GMain.Main.quit) ];
      "Settings", settings;
    ]
    in
    let create_menu ~packing (label, entries) =
      let item = GMenu.menu_item ~label ~packing () in
      let menu = GMenu.menu ~packing:item#set_submenu () in
      GToolbox.build_menu menu ~entries
    in
    let menubar = GMenu.menu_bar ~packing () in
    ignore (List.map (create_menu ~packing:menubar#append) menu)
end

let mk_interface () =
  let window = GWindow.window ~allow_shrink:true ~width:800 ~height:480 () in
  ignore (window#connect#destroy ~callback:GMain.Main.quit);
  let vbox = GPack.vbox ~packing:window#add () in
  UI.menu ~packing:vbox#pack;
  let button_box = GButton.toolbar ~packing:vbox#pack () in
  let process_btn = button_box#insert_button ~text:"Process" () in
  let update_btn =
    let btn = button_box#insert_button ~text:"Update" () in
    let cb_id = ref None in
    fun ~callback ->
      Gaux.may (GtkSignal.disconnect btn#as_widget) !cb_id;
      cb_id := Some (btn#connect#clicked ~callback)
  in
  ignore (GMain.Timeout.add ~ms:50 ~callback:(fun () ->
    ignore (new UI.core ~update_btn ~process_btn ~packing:(vbox#pack ~expand:true));
    false));
  window#show ()

let () =
  Printexc.record_backtrace true;
  ignore (GtkMain.Main.init ());
  let b = Buffer.create 1000 in
  Yypkg_top.main_wrap_wrap b;
  if Buffer.contents b <> ""  then
    (let dialog = GWindow.dialog ~title:"Exception raised" () in
    ignore (GMisc.label ~packing:dialog#vbox#add ~text:(Buffer.contents b) ());
    dialog#add_button_stock `OK `DELETE_EVENT;
    ignore (dialog#run ());
    dialog#destroy ())
  else
    (mk_interface ();
    GMain.Main.main ())
