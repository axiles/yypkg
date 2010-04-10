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
  | AHK p -> (id, (Filelist (command (String.concat " " (ahk_bin :: p)))))
  | (* quote *) Expand (i, p) -> (id, (Filelist (expand package i p)))
  | Exec p -> (id, (Filelist (command (String.concat " " p))))
  | (* quote *) MKdir p -> (id, (Filelist (mkdir p)))
  
let install_package package (conf : predicates) db =
  let (((metadata, install_actions, _) as script)) = open_package package in
  let (_, f_preds) =
    List.partition (predicate_holds conf) metadata.predicates
  in
    if 0 = (List.length f_preds)
    then
      (let results =
         List.map (execute_install_action package) install_actions in
       let updated_db = Db.install_package db (script, (List.rev results))
       in updated_db)
    else raise (Unmatched_predicates f_preds)
  
let install p conf db =
  let p = FilePath.DefaultPath.make_absolute Lib.install_path p
  in
    ((* check the file exists, may raise 'File_not_found p' : caught in yypkg.ml *)
     assert_file_exists p;
     (* if the line above didn't abort, go on and instal the package *)
     install_package p conf db)
  

