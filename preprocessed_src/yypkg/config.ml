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
(* splits the string "A=X" into "A","X" and then updates the association list *)
let setpred conf pred =
  let key_value_pair s =
    let l = String.length s in
    let i = String.index s '=' in
    let key = String.sub s 0 i in
    let value =
      Str.split (Str.regexp ",") (String.sub s (i + 1) ((l - i) - 1))
    in (key, value) in
  let pred = key_value_pair pred in Conf.set conf pred
  
let delpred conf pred = Conf.unset conf pred
  

