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

let execute_install_action package = function
  | Expand (in_, p) -> expand package in_ p
  | Exec p -> command p
  | MKdir p -> [ mkdir p ]
  | SearchReplace (p, search, replace) ->
      let replace = expand_environment_variables replace in
      Lib.search_and_replace_in_file p search replace;
      []
  | Symlink (target, name, kind) ->
      symlink ~target ~name ~kind;
      [ name ]

let execute_install_action_wrap package (id, action) =
  let can_fail_re = Str.regexp ".*-can-fail" in
  let res =
    try execute_install_action package action with
    | _ when Str.string_match can_fail_re id 0 -> []
  in
  id, res

let install_package package conf db =
  let metadata, install_actions, _ as script = Lib.open_package package in
  let pred_holds = predicate_holds conf.preds in
  let _, false_preds = List.partition pred_holds metadata.predicates in
  if false_preds = [] then
    let func = execute_install_action_wrap package in
    let results = List.rev_map func install_actions in
    let updated_db = Db.install_package db (script, results) in
    updated_db
  else
    raise (Unmatched_predicates false_preds)

let install conf db p =
  Printf.eprintf "Installing %s...%!" (FilePath.DefaultPath.basename p);
  (* check the file exists, may raise 'File_not_found p' : caught in yypkg.ml *)
  Lib.assert_file_exists p;
  (* if the line above didn't abort, go on and instal the package *)
  let updated_db = install_package p conf db in
  Printf.eprintf " DONE\n%!";
  updated_db

let install yypkg_conf l db =
  List.fold_left (install yypkg_conf) db l

