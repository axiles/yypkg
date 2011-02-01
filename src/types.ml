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
open Sexplib.Conv

TYPE_CONV_PATH "Types"

type date =
  int (* year *)
  * int (* month *)
  * int (* day *)
  * int (* hour *)
  * int (* minute *)
with sexp

let string_of_date (year, month, day, hour, minute) =
  Printf.sprintf "%d-%d-%d-%d-%d" year month day hour minute

type status = 
  | Alpha of int
  | Beta of int
  | RC of int
  | Snapshot_date of date
  | Snapshot_hash of string
  | Stable
with sexp

let string_of_status = function
  | Alpha x -> sprintf "alpha-%d" x
  | Beta x -> sprintf "beta-%d" x
  | RC x -> sprintf "rc-%d" x
  | Snapshot_date date -> sprintf "snapshot-%s" (string_of_date date)
  | Snapshot_hash s -> sprintf "snapshot-%s" s
  | Stable -> "stable"

type version = (int list * status * int) with sexp

(* create a string from a version *)
let string_of_version (version, status, iteration) =
  let version = String.concat "." (List.map string_of_int version) in
  let status = string_of_status status in
  String.concat "-" [ version; status; string_of_int iteration ]

let dummy_version () =
  [ 0; 0; 17 ], Snapshot_date ( 1970, 01, 01, 00, 00 ), 1

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
  (* TODO: the string list below should be 'outside_path
   * have to clean up the {in,out}side_path mess *)
  | SearchReplace of string list * string * string
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

type size = FileUtil.size

let size_of_sexp sexp = match sexp with
  | Sexplib.Sexp.List [ Sexplib.Sexp.Atom mult; Sexplib.Sexp.Atom s ] ->
      let i = try Int64.of_string s with Failure "int_of_string" ->
        of_sexp_error "size_of_sexp: size isn't a valid integer" sexp
      in
      begin match mult with
      | "TB" -> FileUtil.TB i | "GB" -> FileUtil.GB i | "MB" -> FileUtil.MB i
      | "KB" -> FileUtil.KB i | "B" -> FileUtil.B i
      | _ -> of_sexp_error "size_of_sexp: invalid size prefix" sexp
      end
  | _ -> of_sexp_error "size_of_sexp: not a FileUtil.size" sexp

let sexp_of_size size =
  let p s i =
    let module S = Sexplib.Sexp in
    S.List [ S.Atom s; S.Atom (Int64.to_string i) ]
  in
  match size with
  | FileUtil.TB i -> p "TB" i
  | FileUtil.GB i -> p "GB" i
  | FileUtil.MB i -> p "MB" i
  | FileUtil.KB i -> p "KB" i
  | FileUtil.B i -> p "B" i

type metadata = {
  name : string;
  size_expanded : size;
  version : version;
  packager_email : string;
  packager_name : string;
  description : string;
  host : string;
  target : string sexp_option;
  predicates : (string * string) list;
  comments : string list;
} with sexp

let dummy_meta () =
  let version = dummy_version () in
  let size_expanded = FileUtil.TB (Int64.of_int 42) in
  let meta = { name = "dummy_name"; size_expanded = size_expanded; version =
    version; packager_email = "adrien@notk.org"; packager_name = "Adrien Nader";
    description = "dummy"; host = "%{HST}"; target = Some "%{TGT}";
    predicates = []; comments = [] }
  in
  Sexplib.Sexp.to_string_hum (sexp_of_metadata meta)

type script =
  metadata
  * (action_id * install_action) list
  * uninstall_action list
with sexp

type package = script * (action_id * results) list with sexp

type db = package list with sexp

(* list of predicates that are checked before installing apackage: for instance:
  * license=bsd
  * stability=stable,release_candidate
 * It's mostly free-form, and left as a way to extend the format easily *)
type conf = {
  preds : predicates;
} with sexp

type field = 
  | Predicate of (string * string list)

type pkg = {
  metadata : metadata;
  size_compressed : size;
  filename : string;
  signature : string option;
  files : string list;
  deps : string list;
} with sexp

type repo = {
  repo_target : string;
  pkglist : pkg list;
} with sexp

type sherpa_conf = {
  mirror : string;
  sherpa_version : string;
  download_folder : string;
  arch : string;
} with sexp

type sherpa_conf_field =
  | Mirror of string

exception Package_does_not_exist
exception File_not_found of string
exception Not_upgrading_non_installed_package of string

