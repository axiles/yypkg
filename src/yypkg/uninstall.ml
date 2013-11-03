(*
 * yypkg - A cross-platform package manager
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

let file_can_be_removed file other_packages =
  (* not (List.exists (file_exists_in_package file) other_packages) *)
  if List.exists (file_exists_in_package file) other_packages then (
    Lib.ep "Not removed (exists in another package): %s\n" file;
    false)
  else
    true

let execute_uninstall_action other_pkgs pkg =
  let (_, _, uninstall_actions), install_results = pkg in
  ListLabels.iter uninstall_actions ~f:(function
    | RM p -> assert false
    (* id, "" *)
    | Reverse id ->
        (* rm the files that have been added by 'id' *)
        let pred (action_id, results) = action_id = id in
        ListLabels.iter (List.find_all pred install_results) ~f:(fun (id, l) ->
          (* We have to reverse the actions: files are created first and then,
           * contents of the folder. Of course, when removing, we have to remove
           * in reverse order: contents and then folder *)
          ListLabels.iter (List.rev l) ~f:(fun s ->
            if file_can_be_removed s other_pkgs then rm s)
        )
  )

let uninstall_package db package_name =
  let pkgs, other_pkgs = List.partition (package_is_named package_name) db in
  List.iter (execute_uninstall_action other_pkgs) pkgs;
  Db.uninstall_package db package_name

let uninstall db p =
  if is_installed db p then
    uninstall_package db p
  else
    raise Package_does_not_exist

let uninstall l db =
  List.fold_left uninstall db l
