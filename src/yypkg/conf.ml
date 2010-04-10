(*
 * yypkg - A cross-platforma package manager
 * Copyright (C) 2010 Adrien Nader
 * Copyright (C) <year>  <name of author>
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
  let conf = List.remove_assoc binding conf in
  (binding, value) :: conf

let unset conf binding = 
  List.remove_assoc binding conf

let read () =
  conf_of_sexp (Disk.read conf_path)

let write conf =
  (* Let's sort the configuration. Won't be faster but should be nicer to read
   * when editing the file by hand. It'll also avoid requiring to sort the
   * output when listing the configuration on command-line.
   * We use stable_sort so not to change anything if there are several bindings
   * for the same value, it shouldn't happen but Murphy's Law is Murphy's Law,
   * so why not stay safe? *)
  let sorted_conf = List.stable_sort compare conf in
  Disk.write conf_path (sexp_of_conf sorted_conf)

(* read the conf, run the function, write the database to disk
 * if fail raises an exception, nothing will be written :-) *)
let update f =
  write (f (read ()))
