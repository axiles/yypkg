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

let execute_install_action package (id, action) =
  match action with
  | AHK p -> id, Filelist (command (ahk_bin :: p))
  | Expand (in_, p) -> id, Filelist (expand package in_ p)
  | Exec p -> id, Filelist (command p)
  | MKdir p -> id, Filelist (mkdir p)

let install_package package conf db =
  let (metadata, install_actions, _ as script) = Lib.open_package package in
  let pred_holds = Config.predicate_holds conf.preds in
  let _, false_preds = List.partition pred_holds metadata.predicates in
  if false_preds = [] then
    let func = execute_install_action package in
    let results = List.rev_map func install_actions in
    let updated_db = Db.install_package db (script, results) in
    updated_db
  else
    raise (Unmatched_predicates false_preds)

let install conf db p =
  (* check the file exists, may raise 'File_not_found p' : caught in yypkg.ml *)
  Lib.assert_file_exists p;
  (* if the line above didn't abort, go on and instal the package *)
  install_package p conf db

let install yypkg_conf l db =
  List.fold_left (install yypkg_conf) db l

