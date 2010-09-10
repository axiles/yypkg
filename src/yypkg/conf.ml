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

let set conf = function
  | Predicate (binding, value) -> 
      let preds = List.remove_assoc binding conf.preds in
      { conf with preds = (binding, value) :: preds }

let unset conf binding = 
  { conf with preds = List.remove_assoc binding conf.preds }

let read () =
  conf_of_sexp (Disk.read conf_path)

let write conf =
  (* Let's sort the configuration. Won't be faster but should be nicer to read
   * when editing the file by hand. It'll also avoid requiring to sort the
   * output when listing the configuration on command-line.
   * We use stable_sort so not to change anything if there are several bindings
   * for the same value, it shouldn't happen but Murphy's Law is Murphy's Law,
   * so why not stay safe? *)
  let sorted_preds = List.stable_sort compare conf.preds in
  let conf = { conf with preds = sorted_preds } in
  Disk.write conf_path (sexp_of_conf conf)

(* read the conf, run the function, write the database to disk
 * if fail raises an exception, nothing will be written :-) *)
let update f =
  write (f (read ()))

let print_preds conf = 
  let print_single_pred (binding, values) = 
    Printf.printf "%s = %s\n" binding (String.concat "," values)
  in
  List.iter print_single_pred conf.preds

