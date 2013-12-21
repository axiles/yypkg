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

open Types
open Yylib

let install_package db script = 
  script :: db

let uninstall_package db name = 
  List.filter (fun s -> not (package_is_named name s)) db

let read () =
  TypesSexp.To.db (Disk.read db_path)

let write db =
  (* We sort the db because, err, no reason, it won't even be more readable
   * considering the size of the database but in the case one has to edit the db
   * by hand, it's always nicer to have it sorted.
   * We want to use stable_sort to keep the (perfectly fine) old behaviour when
   * there are multiple packages with the same name.
   * We DO NOT WANT to sort the content of each package as it may (and actually
   * will) have unexpected consequences upon package removal *)
  let sorted_db = List.stable_sort compare db in
  Disk.write db_path (TypesSexp.Of.db sorted_db)

(* read the database, run the function, write the database to disk
 * if fail raises an exception, nothing will be written :-) *)
let update (f : db -> db) =
  write (f (read ()))

