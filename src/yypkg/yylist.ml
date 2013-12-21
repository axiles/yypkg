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
open Lib
open Yylib

let print_package p =
  let metadata = metadata_of_pkg p in
  Printf.printf "%s : %s\n" metadata.name (string_of_version metadata.version)

let list db = function
  | [] -> List.iter print_package db
  | l ->
      let l = List.map (fun s -> Str.regexp ("^" ^ s ^ "$")) l in
      let l = List.concat (List.map (Yylib.find_all_by_name_regex db) l) in
      List.iter print_package l

