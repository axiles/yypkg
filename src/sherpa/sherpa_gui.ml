open Types
open Lib
open Sherpalib

type col = {
  installed : string * bool GTree.column;
  selected : string * bool GTree.column;
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

let set_conf_field prop ~parent =
  let conf = read () in
  let text, f = match prop with
  | `mirror -> conf.mirror, fun mirror conf -> { conf with mirror = mirror }
  | `version -> conf.sherpa_version, fun version conf -> { conf with sherpa_version = version }
  in
  let ok_callback dialog textfield () =
    update (f textfield#text); dialog#destroy ()
  in
  set_dialog ~text ~callback:ok_callback ~parent

let cols = new GTree.column_list

let columns = {
  installed = "Installed", cols#add Gobject.Data.boolean;
  selected = "Future", cols#add Gobject.Data.boolean;
  name = "Name", cols#add Gobject.Data.string;
  version_inst = "Version (installed)", cols#add Gobject.Data.string;
  version_avail = "Version (available)", cols#add Gobject.Data.string;
  size_installed = "Size (installed)", cols#add Gobject.Data.string;
  size_package = "Size (package)", cols#add Gobject.Data.string;
  description = "Description", cols#add Gobject.Data.string;
}
let x = columns
let columns_l2 = [ x.name; x.version_inst; x.version_avail; x.size_installed; x.size_package; x.description ]

let textview () =
  let scrolled = GBin.scrolled_window ~hpolicy ~vpolicy () in
  let view = GText.view ~packing:scrolled#add_with_viewport () in
  view, scrolled#coerce

let update_listview ~(model : GTree.tree_store) db pkglist =
  let fill columns db pkg =
    let metadata = pkg.metadata in
    let name = metadata.Types.name in
    let iter = model#append () in
    let version = string_of_version metadata.version in
    let size = FileUtil.string_of_size ~fuzzy:true metadata.size_expanded in
    let installed = List.exists (Yylib.package_is_named name) db in
    model#set ~row:iter ~column:(snd columns.selected) installed;
    model#set ~row:iter ~column:(snd columns.name) name;
    model#set ~row:iter ~column:(snd columns.size_installed) size;
    model#set ~row:iter ~column:(snd columns.version_avail) version;
    model#set ~row:iter ~column:(snd columns.description) metadata.Types.description
  in
  List.iter (fill columns db) pkglist

let listview () =
  let scrolled = GBin.scrolled_window ~hpolicy ~vpolicy () in
  let model = GTree.tree_store cols in
  let treeview = GTree.view ~model ~packing:scrolled#add_with_viewport () in
  let renderer_text = GTree.cell_renderer_text [] in
  let toggle col treepath =
    let iter = model#get_iter treepath in
    model#set ~row:iter ~column:col (not (model#get ~row:iter ~column:col))
  in
  let column_toggle ~tie (title, col) =
    let renderer_toggle = GTree.cell_renderer_toggle [] in
    if tie then ignore (renderer_toggle#connect#toggled ~callback:(toggle col));
    GTree.view_column ~title ~renderer:(renderer_toggle, [ "active", col ]) ()
  in
  let column_string (title, col) =
    GTree.view_column ~title ~renderer:(renderer_text, [ "text", col ]) ()
  in
  let inst = column_toggle ~tie:true x.installed in
  let sel = column_toggle ~tie:false x.selected in
  let columns = inst :: sel :: List.map column_string columns_l2 in
  ignore (List.map treeview#append_column columns); (* NOTE: ignore or not? *)
  model, scrolled#coerce

let menu window listview =
  let update () =
    update_listview ~model:listview (Db.read ()) (pkglist ())
  in
  let file = [ `I ("_Quit", GMain.Main.quit) ] in
  let package_list = [
    `I ("_Search for an update", update);
    `I ("_Force an update", update);
  ]
  in
  let deps = [ `I ("_Mark dependencies", (fun () -> ())); ] in
  let settings = [
    `I ("Set _mirror", set_conf_field `mirror ~parent:window);
    `I ("Set _version", set_conf_field `version ~parent:window);
  ]
  in
  let help = [ `I ("_Help", (fun () -> ())) ] in
  let menu = [
    "_File", file;
    "_Package list", package_list;
    "_Dependencies", deps;
    "_Settings", settings;
    "_Help", help;
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
  let window = GWindow.window () in
  ignore (window#connect#destroy ~callback:GMain.Main.quit);
  window

let interface () =
  let window = window () in
  let vbox = GPack.vbox ~packing:window#add () in
  let paned = GPack.paned `VERTICAL () in
  let listview, listview_scrolled = listview () in
  let textview, textview_scrolled = textview () in
  let menubar = menu window listview in
  paned#add1 listview_scrolled;
  paned#add2 textview_scrolled;
  vbox#pack ~expand:false menubar#coerce;
  vbox#pack ~expand:true paned#coerce;
  { window = window; paned = paned; listview = listview; textview = textview;
    menubar = menubar }

let () =
  Yypkg.main ();
  let interface = interface () in
  interface.window#show ();
  GMain.Main.main ()
