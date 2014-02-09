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

let set conf (binding, value) =
  let predicates = List.remove_assoc binding conf.predicates in
  { conf with predicates = (binding, value) :: predicates }

let unset conf binding = 
  { conf with predicates = List.remove_assoc binding conf.predicates }

let read () =
  TypesSexp.To.conf (Disk.read conf_path)

let write conf =
  (* Let's sort the predicates. Won't be faster but should be nicer to read
   * when editing the file by hand. It'll also avoid requiring to sort the
   * output when listing the configuration to the user.
   * We use stable_sort so not to change anything if there are several bindings
   * for the same value. *)
  let conf = { conf with predicates = List.stable_sort compare conf.predicates } in
  Disk.write conf_path (TypesSexp.Of.conf conf)

(* read the conf, run the function, write the database to disk
 * if fail raises an exception, nothing will be written :-) *)
let update f =
  write (f (read ()))

let print_predicates conf = 
  let print_single_pred (binding, values) = 
    Printf.printf "%s = %s\n" binding (String.concat "," values)
  in
  List.iter print_single_pred conf.predicates

