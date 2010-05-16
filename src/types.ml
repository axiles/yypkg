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

open Printf

TYPE_CONV_PATH "Types"

(* NOTE: this has to be kept ordered !!! *)
type status = 
  | Alpha of int
  | Beta of int
  | RC of int
  | Snapshot of string
  | Stable

type version = {
  major : int;
  minor : int;
  release : int;
  status : status;
  package_iteration : int;
}

(* both don't behave the same way of course *)
type tar_kind = BSD | GNU with sexp

(* not really used right now, might well be dropped in the future
 * I think I've even forgotten why I wanted to have different types for them
 * (well, for safety of course, but what exactly ? *)
type absolute_path = string with sexp
type relative_path = string with sexp

(* paths outside the package, on the hard drive *)
type outside_path = string with sexp

(* paths inside the package *)
type inside_path = relative_path with sexp

type param = string with sexp
type params = string list with sexp
type argv = string list with sexp

(* this is only a name, an identifier *)
type action_id = string with sexp

type install_action =
  | AHK of params
  | Exec of argv
  | Expand of inside_path * outside_path
  | MKdir of outside_path
with sexp

type uninstall_action =
  | RM of outside_path
  | Reverse of action_id
with sexp

type results = 
  | Filelist of string list
  | NA
with sexp

type predicate = string * string list with sexp
type predicates = predicate list with sexp

exception Unmatched_predicates of ((string * string) list)

type metadata = {
  package_name : string;
  package_size_expanded : string;
  package_version : string;
  packager_email : string;
  packager_name : string;
  description : string;
  predicates : (string * string) list;
  comments : string;
} with sexp

type script =
  metadata
  * (action_id * install_action) list
  * uninstall_action list
with sexp

type package = script * (action_id * results) list with sexp

type db = package list with sexp

(* list of predicates that are checked before installing apackage: for instance:
  * arch=x86_64,noarch
  * stability=stable,release_candidate *)
type conf = {
  preds : predicates;
  tar_kind : tar_kind;
} with sexp

type field = 
  | Predicate of (string * string list)
  | Tar_kind of tar_kind
