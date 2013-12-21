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

(* Split a string "X=A,B,C" into (X, [A; B; C]) *)
let key_value_pair s =
  let l = String.length s in
  let i = String.index s '=' in
  let key = String.sub s 0 i in
  let value = Str.split (Str.regexp ",") (String.sub s (i+1) (l-i-1)) in
  key, value

(* updates the association list from "X=A,B,C" strings *)
let setpred conf pred =
  Conf.set conf (key_value_pair pred)

let delpred conf pred =
  Conf.unset conf pred

(* check if the predicate holds against conf *)
let predicate_holds (conf : predicate list) (key, value) = 
  (* List.assoc may raise Not_found: means the predicate hasn't been set in the
   * configuration, equivalent to false *)
  try 
    let conf_vals = List.assoc key conf in
    List.mem value conf_vals
  with Not_found -> false

(* Nothing to do right now *)
(* let regen _ = () *)
