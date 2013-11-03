(*
 * yypkg - A cross-platform package manager
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

type source_version = 
  | Stable of string
  | RC of string
  | Beta of string
  | Alpha of string
  | Snapshot of string

let string_of_source_version = function
  | Alpha s -> "alpha-" ^ s
  | Beta s -> "beta-" ^ s
  | RC s -> "rc-" ^ s
  | Snapshot s -> "snapshot-" ^ s
  | Stable s -> "stable-" ^ s

type version = source_version * int

(* create a string from a version *)
let string_of_version (source_version, iteration) =
  Printf.sprintf "%s-%d" (string_of_source_version source_version) iteration

let dummy_version () =
  Snapshot "1970-01-01-00-00", 1

(* this is only a name, an identifier *)
type action_id = string
type filekind = [ `Directory | `File | `Unhandled of string ]
type install_action =
  | AHK of string list (* params *)
  | Exec of string list (* argv *)
  | Expand of string * string (* extract from X to Y: from archive to system *)
  | MKdir of string (* mkdir X, path on the system *)
  | SearchReplace of string * string * string (* file, search, replace *)
  | Symlink of string * string * filekind (* target, name, filekind *)

type result = string list

type uninstall_action =
  | RM of string (* rm X, path on the system *)
  | Reverse of action_id (* remove files added by action action_id *)

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
  target : string option;
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

module SherpaT = struct
  type repo = {
    target : string;
    host : string;
    pkglist : pkg list;
  }

  type sherpa_conf = {
    mirror : string;
    download_folder : string;
  }

  type sherpa_conf_field =
    | Mirror of string
end

exception Package_does_not_exist
exception File_not_found of string
exception Not_upgrading_not_installed_package of string

