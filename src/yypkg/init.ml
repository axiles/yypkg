(*
 * yypkg - A cross-platform package manager
 * Copyright (C) 2010-2014 Adrien Nader
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(* Init a yypkg installation in the given folder, this means:
  * create /etc and an empty conf in /etc/yypkg.conf
  * create /sbin
  * create /var/log/packages and an empty db in /var/log/packages/yypkg_db
  * put the required binaries in /sbin (xz, (bsd)tar,...) *)

open Types
open Yylib

let is_binary x =
  let exts = [ "dll"; "exe" ] in
  List.fold_left (fun b e -> b || FilePath.check_extension x e) false exts

(* we Sys.chdir to prefix but also need the value of prefix for make_absolute *)
let init prefix =
  (* On windows, we need an absolute filename it seems *)
  let mk_absolute p = FilePath.DefaultPath.make_absolute prefix p in
  let dl_folder = mk_absolute default_download_path in
  let folders = [conf_folder; "sbin"; db_folder] in
  let folders = dl_folder :: (List.map mk_absolute folders) in
  List.iter (fun f -> FileUtil.mkdir ~parent:true ~mode:0o755 f) folders;
  Sys.chdir prefix;
  (if `Windows = Lib.os_type then
    let module FU = FileUtil in
    let fl = FU.find FU.Is_file Lib.binary_path (Lib.prepend_if is_binary) [] in
    FU.cp fl (mk_absolute "sbin"));
  Disk.write db_path (TypesSexp.Of.db []);
  let base_conf = { preds = [] } in
  let base_sherpa_conf = {
    SherpaT.mirror = "http://yypkg.org/VERSION/packages/SERIES";
    download_folder = dl_folder;
  }
  in
  Disk.write conf_path (TypesSexp.Of.conf base_conf);
  Disk.write sherpa_conf_path (TypesSexp.Of.sherpa_conf base_sherpa_conf)
