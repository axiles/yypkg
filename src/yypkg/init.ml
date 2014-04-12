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
  * create /var/log/packages and an empty db in /var/log/packages/yypkg_db *)

open Types
open Yylib

(* we Sys.chdir to prefix but also need the value of prefix for make_absolute *)
let init prefix =
  (* TODO: check that we don't overwrite any file that would already exist. *)
  let mkdir p = FileUtil.mkdir ~parent:true ~mode:0o755 p in
  let yypkg = FilePath.DefaultPath.make_absolute (Sys.getcwd ()) Sys.argv.(0) in
  mkdir prefix;
  Sys.chdir prefix;
  List.iter mkdir [ default_download_path; conf_dir; db_dir ];
  FileUtil.cp [ yypkg ] "bin";
  Disk.write db_path (TypesSexp.Of.db []);
  Disk.write conf_path (TypesSexp.Of.conf { mirror = ""; predicates = [] })
