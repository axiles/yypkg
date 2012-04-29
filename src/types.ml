(*
 * yypkg - A cross-platforma package manager
 * Copyright (C) 2010 Adrien Nader
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

type date =
  int (* year *)
  * int (* month *)
  * int (* day *)
  * int (* hour *)
  * int (* minute *)

let string_of_date (year, month, day, hour, minute) =
  Printf.sprintf "%d-%d-%d-%d-%d" year month day hour minute

type status = 
  | Alpha of int
  | Beta of int
  | RC of int
  | Snapshot_date of date
  | Snapshot_hash of string
  | Stable

let string_of_status = function
  | Alpha x -> Printf.sprintf "alpha-%d" x
  | Beta x -> Printf.sprintf "beta-%d" x
  | RC x -> Printf.sprintf "rc-%d" x
  | Snapshot_date date -> Printf.sprintf "snapshot-%s" (string_of_date date)
  | Snapshot_hash s -> Printf.sprintf "snapshot-%s" s
  | Stable -> "stable"

type version = (int list * status * int)

(* create a string from a version *)
let string_of_version (version, status, iteration) =
  let version = String.concat "." (List.map string_of_int version) in
  let status = string_of_status status in
  String.concat "-" [ version; status; string_of_int iteration ]

let dummy_version () =
  [ 0; 0; 17 ], Snapshot_date ( 1970, 01, 01, 00, 00 ), 1

(* this is only a name, an identifier *)
type action_id = string
type install_action =
  | AHK of string list (* params *)
  | Exec of string list (* argv *)
  | Expand of string * string (* extract from X to Y: from archive to system *)
  | MKdir of string (* mkdir X, path on the system *)

type result = 
  | Filelist of string list
  | NA (* XXX: what is this used for? *)

type uninstall_action =
  | RM of string (* RM X, path on the system *)
  | Reverse of action_id

type predicate = string * string list

exception Unmatched_predicates of ((string * string) list)

type size = FileUtil.size

type metadata = {
  name : string;
  size_expanded : size;
  version : version;
  packager_email : string;
  packager_name : string;
  description : string;
  host : string;
  target : string Sexplib.Conv.sexp_option;
  predicates : (string * string) list;
  comments : string list;
}

type script =
  metadata
  * (action_id * install_action) list
  * uninstall_action list

type package = script * (action_id * result) list

type db = package list

(* list of predicates that are checked before installing apackage: for instance:
  * license=bsd
  * stability=stable,release_candidate
 * It's mostly free-form, and left as a way to extend the format easily *)
type conf = {
  preds : predicate list;
}

type field = 
  | Predicate of (string * string list)

type pkg = {
  metadata : metadata;
  size_compressed : size;
  filename : string;
  signature : string option;
  files : string list;
  deps : string list;
}

type repo = {
  repo_target : string;
  pkglist : pkg list;
}

type sherpa_conf = {
  mirror : string;
  sherpa_version : string;
  download_folder : string;
  arch : string;
}

type sherpa_conf_field =
  | Mirror of string

exception Package_does_not_exist
exception File_not_found of string
exception Not_upgrading_not_installed_package of string

