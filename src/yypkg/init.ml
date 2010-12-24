(*
 * yypkg - A cross-platforma package manager
 * Copyright (C) 2010 Adrien Nader
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

let mkdir =
  FileUtil.mkdir ~parent:true ~mode:0o755

(* we Sys.chdir to prefix but also need the value of prefix for make_absolute *)
let init prefix =
  (* On windows, we need an absolute filename it seems *)
  let mk_absolute prefix p = FilePath.DefaultPath.make_absolute prefix p in
  let dl_folder = mk_absolute prefix default_download_path in
  let folders = [conf_folder; "sbin"; db_folder] in
  let binaries = [ "NamedPipe.exe"; "bsdtar.exe"; "liblzma-0.dll"; "yypkg.exe";
  "makeypkg.exe"; "wget.exe"; "sherpa_gui.exe"; "sherpa_gen.exe"; "sherpa.exe" ] in
  let folders = dl_folder :: (List.map (mk_absolute prefix) folders) in
  let binaries = List.map (mk_absolute Lib.binary_path) binaries in
  List.iter mkdir folders;
  Sys.chdir prefix;
  (* XXX: FileUtil.cp has some optional arguments but I'm not sure what they
   * default to *)
  (if "Win32" = Sys.os_type then FileUtil.cp binaries (mk_absolute prefix "sbin"));
  Disk.write db_path (sexp_of_db []);
  let base_conf = { preds = [] } in
  let base_sherpa_conf = {
    mirror = "http://notk.org/~adrien/yypkg";
    sherpa_version = "latest";
    download_folder = dl_folder;
  }
  in
  Disk.write conf_path (sexp_of_conf base_conf);
  Disk.write sherpa_conf_path (sexp_of_sherpa_conf base_sherpa_conf)
