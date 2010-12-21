open Types
open Sherpalib
open Lib

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
}

let hpolicy = `AUTOMATIC
let vpolicy = `AUTOMATIC

let cols = new GTree.column_list

let columns = {
  installed = "Installed", cols#add Gobject.Data.boolean;
  selected = "Auto-selected", cols#add Gobject.Data.boolean;
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
    let size = FileUtil.string_of_size metadata.size_expanded in
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

let window () =
  let window = GWindow.window () in
  ignore (window#connect#destroy ~callback:GMain.Main.quit);
  window

let interface () =
  let window = window () in
  let paned = GPack.paned `VERTICAL ~packing:window#add () in
  let listview, listview_scrolled = listview () in
  let textview, textview_scrolled = textview () in
  paned#add1 listview_scrolled;
  paned#add2 textview_scrolled;
  { window = window; paned = paned; listview = listview; textview = textview }

let () =
  Yypkg.main ();
  let interface = interface () in
  interface.window#show ();
  update_listview ~model:interface.listview (Db.read ()) (pkglist_of_uri pkg_list_uri);
  GMain.Main.main ()
