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

type interface = {
  window : GWindow.window;
  paned : GPack.paned;
  listview : GTree.tree_store;
  textview : GText.view;
  menubar : GMenu.menu_shell;
}

let pkglist = lazy (ref (pkglist ()))

let hpolicy = `AUTOMATIC
let vpolicy = `AUTOMATIC

let set_dialog ~text ~callback ~parent () =
  let dialog = GWindow.dialog ~parent ~destroy_with_parent:true ~show:true () in
  let textfield = GEdit.entry ~width:400 ~text () in
  let hbox = GPack.hbox () in
  let ok = GButton.button ~stock:`OK ~packing:hbox#add ~show:true () in
  let cancel = GButton.button ~stock:`CANCEL ~packing:hbox#add ~show:true () in
  ignore (ok#connect#clicked ~callback:(callback dialog textfield));
  ignore (cancel#connect#clicked ~callback:dialog#destroy);
  dialog#vbox#pack ~expand:false textfield#coerce;
  dialog#vbox#pack ~expand:false hbox#coerce

let set_conf_field ~read ~update ~text ~f ~parent =
  let conf = read () in
  let text = text conf in
  let ok_callback dialog textfield () =
    update (f textfield#text); dialog#destroy ()
  in
  set_dialog ~text ~callback:ok_callback ~parent

let set_sherpa_conf_field prop ~parent =
  let text, f = match prop with
  | `mirror -> (fun c -> c.mirror), fun mirror c -> { c with mirror = mirror }
  | `version -> (fun c -> c.sherpa_version), fun version c -> { c with sherpa_version = version }
  | `downloadfolder -> (fun c -> c.download_folder), fun folder c -> { c with download_folder = folder }
  in
  set_conf_field ~read ~update ~text ~f ~parent

let set_yypkg_conf_field prop ~parent =
  let pred_text pred c =
    try String.concat "," (List.assoc pred c.preds) with Not_found -> ""
  in
  let text, f = match prop with
  | `pred p -> pred_text p, fun s c -> Config.setpred c (p ^ "=" ^ s)
  in
  set_conf_field ~read:Conf.read ~update:Conf.update ~text ~f ~parent

let cols = new GTree.column_list

let columns = {
  installed = "Selected", cols#add Gobject.Data.boolean;
  with_deps = "Deps included", cols#add Gobject.Data.boolean;
  name = "Name", cols#add Gobject.Data.string;
  version_inst = "Version (installed)", cols#add Gobject.Data.string;
  version_avail = "Version (available)", cols#add Gobject.Data.string;
  size_installed = "Size (installed)", cols#add Gobject.Data.string;
  size_package = "Size (package)", cols#add Gobject.Data.string;
  description = "Description", cols#add Gobject.Data.string;
}
let x = columns
let columns_l2 = [ x.name; x.version_inst; x.version_avail; x.size_installed; x.size_package; x.description ]

let details_of_package pkg =
  let buffer = GText.buffer () in
  let field_tag = buffer#create_tag ~name:"field" [ `WEIGHT `BOLD ] in
  buffer#insert ~tags:[field_tag] "Description";
  buffer#insert ": ";
  buffer#insert pkg.metadata.Types.description;
  buffer

let textview () =
  let scrolled = GBin.scrolled_window ~hpolicy ~vpolicy () in
  let view = GText.view ~editable:false ~packing:scrolled#add_with_viewport () in
  view, scrolled#coerce

let rec partition model (vrai, faux) pred f g iter =
  let accu = if pred model iter then
    ((f model iter) :: vrai), faux
  else
    vrai, ((g model iter) :: faux)
  in
  if model#iter_next iter then partition model accu pred f g iter else accu

let selection_changed_cb ~pkglist ~textview ~(model : GTree.tree_store) ~selection () =
  match selection#get_selected_rows with
  | path :: _ ->
      let iter = model#get_iter path in
      let name = model#get ~row:iter ~column:(snd columns.name) in
      let pkg = find_by_name pkglist name in
      let buffer = details_of_package pkg in
      textview#set_buffer buffer
  | [] -> ()

let update_listview ~(model : GTree.tree_store) ~(treeview : GTree.view) ~textview db pkglist =
  let fill columns db pkg =
    let metadata = pkg.metadata in
    let name = metadata.Types.name in
    let iter = model#append () in
    let selection = treeview#selection in
    let callback = selection_changed_cb ~pkglist ~textview ~model ~selection in
    ignore (selection#connect#changed ~callback);
    let sherpa_version = string_of_version metadata.version in
    let size = FileUtil.string_of_size ~fuzzy:true metadata.size_expanded in
    let size_pkg = FileUtil.string_of_size ~fuzzy:true pkg.size_compressed in
    model#set ~row:iter ~column:(snd columns.name) name;
    model#set ~row:iter ~column:(snd columns.size_installed) size;
    model#set ~row:iter ~column:(snd columns.size_package) size_pkg;
    model#set ~row:iter ~column:(snd columns.version_avail) sherpa_version;
    model#set ~row:iter ~column:(snd columns.description) metadata.Types.description;
    try
      let m = Yylib.metadata_of_pkg (Yylib.find_by_name db name) in
      model#set ~row:iter ~column:(snd columns.installed) true;
      model#set ~row:iter ~column:(snd columns.with_deps) true;
      model#set ~row:iter ~column:(snd columns.version_inst) (string_of_version m.version)
    with _ -> ()
  in
  List.iter (fill columns db) pkglist

let selecteds_of ~model ~column =
  match model#get_iter_first with
  | Some iter ->
      let is_selected (model : GTree.tree_store) iter =
        model#get ~row:iter ~column
      in
      let f (model : GTree.tree_store) iter =
        model#get ~row:iter ~column:(snd columns.name)
      in
      partition model ([], []) is_selected f f iter
  | None -> [], []

let avail_is_newer_than_installed db p =
  let name = p.metadata.Types.name in
  if Yylib.is_installed db name then
    let pkg = Yylib.find_by_name db name in
    let installed = (Yylib.metadata_of_pkg pkg).version in
    let avail = p.metadata.Types.version in
    avail > installed
  else
    true

let process ~model ~db () =
  let pkglist = !(Lazy.force pkglist) in
  let conf = read () in
  let selecteds, unselecteds = selecteds_of ~model ~column:(snd columns.with_deps) in
  let selecteds = find_all_by_name pkglist selecteds in
  let uninst = List.filter (Yylib.is_installed db) unselecteds in
  let inewer = List.filter (avail_is_newer_than_installed db) selecteds in
  let ipkgs = List.map (download_to_folder conf.download_folder) inewer in
  Db.update (Uninstall.uninstall uninst);
  Db.update (Install.install (Conf.read ()) ipkgs)

let update_listview_deps ~(model : GTree.tree_store) =
  let rec update columns selecteds iter =
    let name = model#get ~row:iter ~column:(snd columns.name) in
    let selected = List.mem name selecteds in
    model#set ~row:iter ~column:(snd columns.with_deps) selected;
    if model#iter_next iter then update columns selecteds iter else ()
  in
  let should_be_uninstalled unselecteds db p =
    Yylib.is_installed db p.metadata.Types.name && List.mem p unselecteds
  in
  let pkglist = !(Lazy.force pkglist) in
  let db = Db.read () in
  let selecteds, unselecteds = selecteds_of ~model ~column:(snd columns.installed) in
  match model#get_iter_first with
  | Some iter ->
      let selecteds = find_all_by_name pkglist selecteds in
      let unselecteds = find_all_by_name pkglist unselecteds in
      let deps = get_deps pkglist selecteds in
      let deps = List.filter (fun p -> not (should_be_uninstalled unselecteds db p)) deps in
      let deps = List.map (fun p -> p.metadata.Types.name) deps in
      update columns deps iter
  | None -> ()

let listview () =
  let scrolled = GBin.scrolled_window ~hpolicy ~vpolicy () in
  let model = GTree.tree_store cols in
  let treeview = GTree.view ~model ~packing:scrolled#add_with_viewport () in
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
  let f = update_listview_deps in
  let inst = column_toggle ~auto:true ~on_toggle:(toggle ~f) x.installed in
  let sel = column_toggle ~auto:true ~on_toggle:toggle x.with_deps in
  let columns = inst :: sel :: List.map column_string columns_l2 in
  ignore (List.map treeview#append_column columns);
  model, treeview, scrolled#coerce

let menu ~window ~model ~treeview ~textview =
  let update () =
    update_listview ~model ~treeview ~textview (Db.read ()) !(Lazy.force pkglist)
  in
  let file = [
    `I ("Process", process ~model ~db:(Db.read ()));
    `I ("Quit", GMain.Main.quit);
    ]
  in
  let package_list = [
    `I ("Force update", update);
  ]
  in
  let predicates = [ `I ("Arch", set_yypkg_conf_field (`pred "arch") ~parent:window); ] in
  let settings = [
    `I ("Mirror", set_sherpa_conf_field `mirror ~parent:window);
    `I ("Version", set_sherpa_conf_field `version ~parent:window);
    `I ("Download folder", set_sherpa_conf_field `downloadfolder ~parent:window);
    `M ("Predicates", predicates);
  ]
  in
  let help = [ `I ("Help", (fun () -> ())) ] in
  let menu = [
    "File", file;
    "Package list", package_list;
    "Settings", settings;
    "Help", help;
  ]
  in
  let create_menu ~packing (label, entries) =
    let item = GMenu.menu_item ~label ~packing () in
    let menu = GMenu.menu ~packing:item#set_submenu () in
    GToolbox.build_menu menu ~entries
  in
  let menubar = GMenu.menu_bar () in
  ignore (List.map (create_menu ~packing:menubar#append) menu);
  menubar

let window () =
  let window = GWindow.window ~width:800 ~height:480 () in
  ignore (window#connect#destroy ~callback:GMain.Main.quit);
  window

let interface () =
  let window = window () in
  let vbox = GPack.vbox ~packing:window#add () in
  let paned = GPack.paned `VERTICAL () in
  let textview, textview_scrolled = textview () in
  let model, treeview, listview_scrolled = listview () in
  let menubar = menu ~window ~model ~treeview ~textview in
  paned#pack1 ~shrink:false listview_scrolled;
  paned#pack2 ~shrink:true textview_scrolled;
  vbox#pack ~expand:false menubar#coerce;
  vbox#pack ~expand:true paned#coerce;
  { window = window; paned = paned; listview = model; textview = textview;
    menubar = menubar }

let () =
  Yypkg.main ();
  ignore (GtkMain.Main.init ());
  let interface = interface () in
  interface.window#show ();
  GMain.Main.main ()
