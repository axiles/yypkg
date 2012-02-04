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

let sexp_of_date (year, month, day, hour, minute) =
  let f = Sexplib.Conv.sexp_of_int in
  Sexplib.Sexp.List [ f year; f month; f day; f hour; f minute ]

let date_of_sexp sexp =
  let f = int_of_sexp in
  match sexp with
  | Sexplib.Sexp.List [ year; month; day; hour; minute ] ->
      (f year, f month, f day, f hour, f minute)
  | _ -> of_sexp_error "data_of_sexp: list of 5 int atoms needed" sexp

let string_of_date (year, month, day, hour, minute) =
  Printf.sprintf "%d-%d-%d-%d-%d" year month day hour minute

type status = 
  | Alpha of int
  | Beta of int
  | RC of int
  | Snapshot_date of date
  | Snapshot_hash of string
  | Stable

let status_of_sexp (sexp : Sexplib.Sexp.t) =
  let open Sexplib.Sexp in
  match sexp with
  | List [ Atom "Alpha"; i ] -> Alpha (int_of_sexp i)
  | List [ Atom "Beta"; i ] -> Beta (int_of_sexp i)
  | List [ Atom "RC"; i ] -> RC (int_of_sexp i)
  | List [ Atom "Snapshot_date"; date ] -> Snapshot_date (date_of_sexp date)
  | List [ Atom "Snapshot_hash"; hash ] -> Snapshot_hash (string_of_sexp hash)
  | List [ Atom "Stable" ] -> Stable
  | _ -> of_sexp_error "status_of_sexp: wrong atom or atom argument" sexp

let sexp_of_status status =
  let open Sexplib.Sexp in
  match status with
  | Alpha i -> List [ Atom "Alpha"; sexp_of_int i ]
  | Beta i -> List [ Atom "Beta"; sexp_of_int i ]
  | RC i -> List [ Atom "RC"; sexp_of_int i ]
  | Snapshot_date date -> List [ Atom "Snapshot_date"; sexp_of_date date ]
  | Snapshot_hash s -> List [ Atom "Snapshot_hash"; sexp_of_string s ]
  | Stable -> Atom "Stable"

let string_of_status = function
  | Alpha x -> sprintf "alpha-%d" x
  | Beta x -> sprintf "beta-%d" x
  | RC x -> sprintf "rc-%d" x
  | Snapshot_date date -> sprintf "snapshot-%s" (string_of_date date)
  | Snapshot_hash s -> sprintf "snapshot-%s" s
  | Stable -> "stable"

type version = (int list * status * int)

let version_of_sexp sexp =
  let open Sexplib.Sexp in
  match sexp with
  | List [ version_components; status; build_number ] ->
      let version_components = list_of_sexp int_of_sexp version_components in
      (version_components, status_of_sexp status, int_of_sexp build_number)
  | List _ -> of_sexp_error
      "version_of_sexp: list must contain exactly three elements" sexp
  | Atom _ -> of_sexp_error "version_of_sexp: list needed" sexp

let sexp_of_version (version_components, status, build_number) =
  let open Sexplib.Sexp in
  List [ sexp_of_list sexp_of_int version_components; sexp_of_status status;
  sexp_of_int build_number ]

(* create a string from a version *)
let string_of_version (version, status, iteration) =
  let version = String.concat "." (List.map string_of_int version) in
  let status = string_of_status status in
  String.concat "-" [ version; status; string_of_int iteration ]

let dummy_version () =
  [ 0; 0; 17 ], Snapshot_date ( 1970, 01, 01, 00, 00 ), 1

let string_list_of_sexp sexp = list_of_sexp string_of_sexp sexp
let sexp_of_string_list params = sexp_of_list sexp_of_string params

(* this is only a name, an identifier *)
type action_id = string
let action_id_of_sexp = string_of_sexp
let sexp_of_action_id = sexp_of_string

type install_action =
  | AHK of string list (* params *)
  | Exec of string list (* argv *)
  | Expand of string * string (* extract from X to Y: from archive to system *)
  | MKdir of string (* mkdir X, path on the system *)
  (* TODO: the string list below should be 'outside_path
   * have to clean up the {in,out}side_path mess *)
  | SearchReplace of string list * string * string

let sexp_of_install_action install_action =
  let open Sexplib.Sexp in
  match install_action with
  | AHK params -> List [ Atom "AHK"; sexp_of_string_list params ]
  | Exec argv -> List [ Atom "Exec"; sexp_of_string_list argv ]
  | Expand (orig, dest) -> List [ Atom "Exec"; Atom orig; Atom dest ]
  | MKdir dir -> List [ Atom "Exec"; Atom dir ]
  | SearchReplace (files, search, replace) ->
      List [ Atom "SearchReplace"; sexp_of_string_list files; Atom search; Atom
      replace ]

let install_action_of_sexp sexp =
  let open Sexplib.Sexp in
  match sexp with
  | List [ Atom "AHK"; params ] -> AHK (string_list_of_sexp params)
  | List [ Atom "Exec"; argv ] -> Exec (string_list_of_sexp argv)
  | List [ Atom "Expand"; List [ orig; dest ] ] ->
      Expand (string_of_sexp orig, string_of_sexp dest)
  | List [ Atom "MKdir"; dir ] -> MKdir (string_of_sexp dir)
  | List [ Atom "SearchReplace"; List [ files; search; replace ] ] ->
      SearchReplace (string_list_of_sexp files, string_of_sexp search,
      string_of_sexp replace)
  | _ -> of_sexp_error "install_action_of_sexp: wrong list or list argument"
  sexp

type uninstall_action =
  | RM of string (* RM X, path on the system *)
  | Reverse of action_id
with sexp

type results = 
  | Filelist of string list
  | NA
with sexp

type predicate = string * string list with sexp
type predicates = predicate list
let predicates_of_sexp sexp = list_of_sexp predicate_of_sexp sexp
let sexp_of_predicates predicates = sexp_of_list sexp_of_predicate predicates

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

type db = package list
let db_of_sexp sexp = list_of_sexp package_of_sexp sexp
let sexp_of_db db = sexp_of_list sexp_of_package db

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
exception Not_upgrading_not_installed_package of string

