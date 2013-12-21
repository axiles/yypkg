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

