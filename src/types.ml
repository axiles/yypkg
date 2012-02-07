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
  | _ -> of_sexp_error "status_of_sexp: wrong atom or wrong atom argument" sexp

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
  | _ -> of_sexp_error
      "install_action_of_sexp: wrong list or wrong list argument" sexp

type uninstall_action =
  | RM of string (* RM X, path on the system *)
  | Reverse of action_id

let uninstall_action_of_sexp sexp =
  let open Sexplib.Sexp in
  match sexp with
  | List [ Atom "RM"; Atom dir ] -> RM dir
  | List [ Atom "Reverse"; Atom action_id ] -> Reverse action_id
  | _ -> of_sexp_error
      "uninstall_action_of_sexp: wrong list or wrong list argument" sexp

let sexp_of_uninstall_action uninstall_action =
  let open Sexplib.Sexp in
  match uninstall_action with
  | RM dir -> List [ Atom "RM"; Atom dir ]
  | Reverse action_id -> List [ Atom "Reverse"; Atom action_id ]

type result = 
  | Filelist of string list
  | NA (* XXX: what is this used for? *)

let result_of_sexp sexp =
  let open Sexplib.Sexp in
  match sexp with
  | List [ Atom "Filelist"; files ] -> Filelist (string_list_of_sexp files)
  | Atom "NA" -> NA
  | _ -> of_sexp_error
      "result_of_sexp: wrong atom, wrong list or wrong list argument" sexp

let sexp_of_result result =
  let open Sexplib.Sexp in
  match result with
  | Filelist files -> List [ Atom "Filelist"; sexp_of_string_list files ]
  | NA -> Atom "NA"

type predicate = string * string list

let predicate_of_sexp sexp =
  let open Sexplib.Sexp in
  match sexp with
  | List [ name; values ] -> string_of_sexp name, string_list_of_sexp values
  | _ -> of_sexp_error
      "predicate_of_sexp: wrong list or wrong list argument" sexp

let sexp_of_predicate (name, values) =
  let open Sexplib.Sexp in
  List [ Atom name; sexp_of_string_list values ]

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
}

let sexp_of_metadata m =
  let open Sexplib.Sexp in
  let f (s1, s2) = List [ sexp_of_string s1; sexp_of_string s2 ] in
  List [
    sexp_of_string m.name;
    sexp_of_size m.size_expanded;
    sexp_of_version m.version;
    sexp_of_string m.packager_email;
    sexp_of_string m.packager_name;
    sexp_of_string m.description;
    sexp_of_string m.host;
    sexp_of_option sexp_of_string m.target;
    sexp_of_list f m.predicates;
    sexp_of_string_list m.comments
  ]

let record_of_sexp_aux ~f_name ~f_sexp ~duplicates ~conv ~res =
  match !res with
  | None -> res := Some (conv f_sexp)
  | Some _ -> duplicates := f_name :: !duplicates

let record_undefined_fields ~name ~l ~sexp =
  let f errs (field, missing) = if missing then field :: errs else errs in
  let missing = List.fold_left f [] l in
  let msg = [ name; "some fields are missing"; String.concat ", " missing ] in
  of_sexp_error (String.concat ": " msg) sexp

let record_extra_fields ~name ~extra ~sexp =
  let msg = [ name; "there are extra fields"; String.concat ", " extra ] in
  of_sexp_error (String.concat ": " msg) sexp

let metadata_of_sexp sexp =
  let open Sexplib.Sexp in
  let n = ref None in
  let name = n and size_expanded = ref None and version = ref None and
  packager_email = n and packager_name = n and description = n and host = n and
  target = n and predicates = ref None and comments = ref None in
  let duplicates = ref [] in
  let extra = ref [] in
  let rec aux = function
    | List [ f_name; f_sexp ] :: q ->
        let f_name = string_of_sexp f_name in
        let f ~conv ~res =
          record_of_sexp_aux ~f_name ~f_sexp ~duplicates ~conv ~res
        in
        let g = function
          | List [ s1; s2 ] -> string_of_sexp s1, string_of_sexp s2
          | _ -> of_sexp_error "" sexp
        in
        (match f_name with
        | "name" -> f ~conv:string_of_sexp ~res:name
        | "size_expanded" -> f ~conv:size_of_sexp ~res:size_expanded
        | "version" -> f ~conv:version_of_sexp ~res:version
        | "package_email" -> f ~conv:string_of_sexp ~res:packager_email
        | "packager_name" -> f ~conv:string_of_sexp ~res:packager_name
        | "description" -> f ~conv:string_of_sexp ~res:description
        | "host" -> f ~conv:string_of_sexp ~res:host
        | "target" -> f ~conv:string_of_sexp ~res:target
        | "predicates" -> f ~conv:(list_of_sexp g) ~res:predicates
        | "comments" -> f ~conv:string_list_of_sexp ~res:comments
        | _ -> extra := f_name :: !extra);
        aux q
    | [] -> (
        match (!name, !size_expanded, !version, !packager_email, !packager_name,
        !description, !host, !target, !predicates, !comments) with
        | Some name, Some size_expanded, Some version, Some packager_email,
          Some packager_name, Some description, Some host, target,
          Some predicates, Some comments ->
            { name = name; size_expanded = size_expanded; version = version;
              packager_email = packager_email; packager_name = packager_name;
              description = description; host = host; target = target;
              predicates = predicates; comments = comments }
        | _ ->
            record_undefined_fields ~name:"metadata_of_sexp" ~sexp ~l:[
              "name", !name = None; "size_expanded", !size_expanded = None;
              "version", !version = None; "packager_email", !packager_email =
              None; "packager_name", !packager_name = None; "description",
              !description = None; "host", !host = None; "predicates",
              !predicates = None; "comments", !comments = None
            ]
      )
    | _ -> of_sexp_error "metadata_of_sexp: atom or wrong list element" sexp
  in
  match sexp with
  | List l -> aux l
  | _ -> of_sexp_error "metadata_of_sexp: atom argument" sexp

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

let sexp_of_script (metadata, install_actions, uninstall_actions) =
  let open Sexplib.Sexp in
  let f (action_id, install_action) =
    List [ sexp_of_action_id action_id; sexp_of_install_action install_action ]
  in
  List [ sexp_of_metadata metadata; sexp_of_list f install_actions;
  sexp_of_list sexp_of_uninstall_action uninstall_actions ]

let script_of_sexp sexp =
  let open Sexplib.Sexp in
  let f = function
    | List [ action_id; install_action ] ->
        action_id_of_sexp action_id, install_action_of_sexp install_action
    | _ -> of_sexp_error
        "predicate_of_sexp: install_actions: atom or wrong list" sexp
  in
  match sexp with
  | List [ metadata; install_actions; uninstall_actions ] ->
      metadata_of_sexp metadata, list_of_sexp f install_actions, list_of_sexp
      uninstall_action_of_sexp uninstall_actions
  | _ -> of_sexp_error "predicate_of_sexp: atom or wrong list" sexp

type package = script * (action_id * result) list

let sexp_of_package (script, results) =
  let open Sexplib.Sexp in
  let f (action_id, result) =
    List [ sexp_of_action_id action_id; sexp_of_result result ]
  in
  List [ sexp_of_script script; sexp_of_list f results ]

let package_of_sexp sexp =
  let open Sexplib.Sexp in
  let f = function
    | List [ action_id; result ] ->
        action_id_of_sexp action_id, result_of_sexp result
    | _ -> of_sexp_error "package_of_sexp: results: atom or wrong list" sexp
  in
  match sexp with
  | List [ script; results ] ->
      script_of_sexp script, list_of_sexp f results
  | _ -> of_sexp_error "package_of_sexp: atom or wrong list" sexp

type db = package list
let db_of_sexp sexp = list_of_sexp package_of_sexp sexp
let sexp_of_db db = sexp_of_list sexp_of_package db

(* list of predicates that are checked before installing apackage: for instance:
  * license=bsd
  * stability=stable,release_candidate
 * It's mostly free-form, and left as a way to extend the format easily *)
type conf = {
  preds : predicate list;
}

let sexp_of_conf conf =
  let open Sexplib.Sexp in
  List [ sexp_of_list sexp_of_predicate conf.preds ]

let conf_of_sexp sexp =
  let open Sexplib.Sexp in
  match sexp with
  | List [ predicates ] -> { preds = list_of_sexp predicate_of_sexp predicates }
  | _ -> of_sexp_error "conf_of_sexp: atom or wrong list" sexp

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

let sexp_of_pkg pkg =
  let open Sexplib.Sexp in
  List [
    sexp_of_metadata pkg.metadata;
    sexp_of_size pkg.size_compressed;
    sexp_of_string pkg.filename;
    sexp_of_option sexp_of_string pkg.signature;
    sexp_of_string_list pkg.files;
    sexp_of_string_list pkg.deps
  ]

let pkg_of_sexp sexp =
  let open Sexplib.Sexp in
  let metadata = ref None and size_compressed = ref None and filename = ref None
  and signature = ref None and files = ref None and deps = ref None in
  let duplicates = ref [] in
  let extra = ref [] in
  let rec aux = function
    | List [ f_name; f_sexp ] :: q ->
        let f_name = string_of_sexp f_name in
        let f ~conv ~res =
          record_of_sexp_aux ~f_name ~f_sexp ~duplicates ~conv ~res
        in
        (match f_name with
        | "metadata" -> f ~conv:metadata_of_sexp ~res:metadata
        | "size_compressed" -> f ~conv:size_of_sexp ~res:size_compressed
        | "filename" -> f ~conv:string_of_sexp ~res:filename
        | "signature" -> f ~conv:(option_of_sexp string_of_sexp) ~res:signature
        | "files" -> f ~conv:string_list_of_sexp ~res:files
        | "deps" -> f ~conv:string_list_of_sexp ~res:deps
        | _ -> extra := f_name :: !extra);
        aux q
    | [] -> (
        match (!metadata, !size_compressed, !filename, !signature, !files,
        !deps) with
        | Some metadata, Some size_compressed, Some filename, Some signature,
          Some files, Some deps ->
            { metadata = metadata; size_compressed = size_compressed; filename =
              filename; signature = signature; files = files; deps = deps }
        | _ ->
            record_undefined_fields ~name:"pkg_of_sexp" ~sexp ~l:[
              "metadata", !metadata = None; "size_compressed", !size_compressed
              = None; "filename", !filename = None; "signature",
              !signature = None; "files", !files = None; "deps", !deps = None
            ]
      )
    | _ -> of_sexp_error "pkg_of_sexp: atom or wrong list element" sexp
  in
  match sexp with
  | List l -> aux l
  | _ -> of_sexp_error "pkg_of_sexp: atom argument" sexp

type repo = {
  repo_target : string;
  pkglist : pkg list;
}

let sexp_of_repo repo =
  let open Sexplib.Sexp in
  List [
    sexp_of_string repo.repo_target;
    sexp_of_list sexp_of_pkg repo.pkglist
  ]

let repo_of_sexp sexp =
  let open Sexplib.Sexp in
  let repo_target = ref None and pkglist = ref None in
  let duplicates = ref [] in
  let extra = ref [] in
  let rec aux = function
    | List [ f_name; f_sexp ] :: q ->
        let f_name = string_of_sexp f_name in
        let f ~conv ~res =
          record_of_sexp_aux ~f_name ~f_sexp ~duplicates ~conv ~res
        in
        (match f_name with
        | "repo_target" -> f ~conv:string_of_sexp ~res:repo_target
        | "pkglist" -> f ~conv:(list_of_sexp pkg_of_sexp) ~res:pkglist
        | _ -> extra := f_name :: !extra);
        aux q
    | [] -> (
        match !repo_target, !pkglist with
        | Some repo_target, Some pkglist ->
            { repo_target = repo_target; pkglist = pkglist }
        | _ -> 
            record_undefined_fields ~name:"repo_of_sexp" ~sexp ~l:[
              "repo_target", !repo_target = None; "pkglist", !pkglist = None ]
      )
    | _ -> of_sexp_error "repo_of_sexp: atom or wrong list element" sexp
  in
  match sexp with
  | List l -> aux l
  | _ -> of_sexp_error "repo_of_sexp: atom argument" sexp

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

module Sexp = struct
end
