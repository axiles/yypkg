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

let set_conf_field prop ~parent =
  let conf = read () in
  let text, f = match prop with
  | `mirror -> conf.mirror, fun mirror conf -> { conf with mirror = mirror }
  | `version -> conf.sherpa_version, fun version conf -> { conf with sherpa_version = version }
  | `downloadfolder -> conf.download_folder, fun folder conf -> { conf with download_folder = folder }
  in
  let ok_callback dialog textfield () =
    update (f textfield#text); dialog#destroy ()
  in
  set_dialog ~text ~callback:ok_callback ~parent

let cols = new GTree.column_list

let columns = {
  installed = "Installed", cols#add Gobject.Data.boolean;
  with_deps = "With deps", cols#add Gobject.Data.boolean;
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
    let sherpa_version = string_of_version metadata.version in
    let size = FileUtil.string_of_size ~fuzzy:true metadata.size_expanded in
    let size_pkg = FileUtil.string_of_size ~fuzzy:true pkg.size_compressed in
    model#set ~row:iter ~column:(snd columns.name) name;
    model#set ~row:iter ~column:(snd columns.size_installed) size;
    model#set ~row:iter ~column:(snd columns.size_package) size_pkg;
    model#set ~row:iter ~column:(snd columns.version_avail) sherpa_version;
    model#set ~row:iter ~column:(snd columns.description) metadata.Types.description;
    try
      let (m, _, _), _ = List.find (Yylib.package_is_named name) db in
      model#set ~row:iter ~column:(snd columns.installed) true;
      model#set ~row:iter ~column:(snd columns.version_inst) (string_of_version m.version)
    with _ -> ()
  in
  List.iter (fill columns db) pkglist

let update_listview_deps ~(model : GTree.tree_store) ~pkglist =
  let rec find accu pred f iter =
    let accu = if pred model iter then (f model iter) :: accu else accu in
    if model#iter_next iter then find accu pred f iter else accu
  in
  let rec update columns selecteds iter =
    let name = model#get ~row:iter ~column:(snd columns.name) in
    let selected = List.mem name selecteds in
    model#set ~row:iter ~column:(snd columns.with_deps) selected;
    if model#iter_next iter then update columns selecteds iter else ()
  in
  let selecteds = match model#get_iter_first with
  | Some iter ->
      let is_selected (model : GTree.tree_store) iter =
        model#get ~row:iter ~column:(snd columns.installed)
      in
      let f (model : GTree.tree_store) iter =
        model#get ~row:iter ~column:(snd columns.name)
      in
      find [] is_selected f iter
  | None -> []
  in
  match model#get_iter_first with
  | Some iter ->
      let selecteds = find_packages_named pkglist selecteds in
      let deps = get_deps pkglist selecteds in
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
  let pkglist = Lazy.force pkglist in
  let f = update_listview_deps ~pkglist:!pkglist in
  let inst = column_toggle ~auto:true ~on_toggle:(toggle ~f) x.installed in
  let sel = column_toggle ~auto:true ~on_toggle:toggle x.with_deps in
  let columns = inst :: sel :: List.map column_string columns_l2 in
  ignore (List.map treeview#append_column columns); (* NOTE: ignore or not? *)
  model, scrolled#coerce

let menu window listview =
  let update () =
    update_listview ~model:listview (Db.read ()) !(Lazy.force pkglist)
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
    `I ("Set _download folder", set_conf_field `downloadfolder ~parent:window);
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
