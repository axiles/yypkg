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

(* Upgrade by installing the new package and then uninstalling the old one. *)
let upgrade ?(install_new=false) yypkg_conf db p =
  assert_file_exists p;
  let metadata = metadata_of_script (open_package p) in
  let pred_holds = predicate_holds yypkg_conf.predicates in
  match List.partition pred_holds metadata.predicates with
  | _, [] -> 
      if is_installed db metadata.name then
        let uninstalled = Uninstall.uninstall [ metadata.name ] db in
        Install.install yypkg_conf [ p ] uninstalled
      else
        if install_new then
          Install.install yypkg_conf [ p ] db
        else
          raise (Not_upgrading_not_installed_package p)
  |_, f_preds -> raise (Unmatched_predicates f_preds)

let upgrade ?install_new yypkg_conf l db =
  List.fold_left (upgrade ?install_new yypkg_conf) db l
