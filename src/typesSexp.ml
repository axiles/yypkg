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

open Sexplib.Sexp
open Sexplib.Conv
open Types

let record_undefined_fields ~name ~l ~sexp =
  let f errs (field, missing) = if missing then field :: errs else errs in
  let missing = List.fold_left f [] l in
  let msg = [ name; "some fields are missing"; String.concat ", " missing ] in
  of_sexp_error (String.concat ": " msg) sexp

let record_extra_fields ~name ~extra ~sexp =
  let msg = [ name; "there are extra fields"; String.concat ", " extra ] in
  of_sexp_error (String.concat ": " msg) sexp

module Of : sig
  val metadata : metadata -> t
  val conf : conf -> t
  val db : db -> t
  val sherpa_conf : sherpa_conf -> t
  val script : script -> t
  val repo : repo -> t
end = struct
  let sexp_of_date (year, month, day, hour, minute) =
    let f = Sexplib.Conv.sexp_of_int in
    Sexplib.Sexp.List [ f year; f month; f day; f hour; f minute ]

  let sexp_of_status status =
    match status with
    | Alpha i -> List [ Atom "Alpha"; sexp_of_int i ]
    | Beta i -> List [ Atom "Beta"; sexp_of_int i ]
    | RC i -> List [ Atom "RC"; sexp_of_int i ]
    | Snapshot_date date -> List [ Atom "Snapshot_date"; sexp_of_date date ]
    | Snapshot_hash s -> List [ Atom "Snapshot_hash"; Atom s ]
    | Stable -> Atom "Stable"

  let sexp_of_version (version_components, status, build_number) =
    List [ sexp_of_list sexp_of_int version_components; sexp_of_status status;
    sexp_of_int build_number ]

  let sexp_of_string_list params = sexp_of_list sexp_of_string params

  let sexp_of_action_id = sexp_of_string

  let sexp_of_install_action install_action =
    match install_action with
    | AHK params -> List [ Atom "AHK"; sexp_of_string_list params ]
    | Exec argv -> List [ Atom "Exec"; sexp_of_string_list argv ]
    | Expand (orig, dest) -> List [ Atom "Exec"; Atom orig; Atom dest ]
    | MKdir dir -> List [ Atom "Exec"; Atom dir ]
    | SearchReplace (files, search, replace) ->
        List [ Atom "SearchReplace"; sexp_of_string_list files; Atom search; Atom
        replace ]

  let sexp_of_uninstall_action uninstall_action =
    match uninstall_action with
    | RM dir -> List [ Atom "RM"; Atom dir ]
    | Reverse action_id -> List [ Atom "Reverse"; Atom action_id ]

  let sexp_of_result result =
    match result with
    | Filelist files -> List [ Atom "Filelist"; sexp_of_string_list files ]
    | NA -> Atom "NA"

  let sexp_of_predicate (name, values) =
    List [ Atom name; sexp_of_string_list values ]

  let sexp_of_size size =
    let p s i =
      List [ Atom s; Atom (Int64.to_string i) ]
    in
    match size with
    | FileUtil.TB i -> p "TB" i
    | FileUtil.GB i -> p "GB" i
    | FileUtil.MB i -> p "MB" i
    | FileUtil.KB i -> p "KB" i
    | FileUtil.B i -> p "B" i

  let sexp_of_metadata m =
    List [
      Atom m.name;
      sexp_of_size m.size_expanded;
      sexp_of_version m.version;
      Atom m.packager_email;
      Atom m.packager_name;
      Atom m.description;
      Atom m.host;
      sexp_of_option sexp_of_string m.target;
      sexp_of_list (fun (s1, s2) -> List [ Atom s1; Atom s2 ])  m.predicates;
      sexp_of_string_list m.comments
    ]

  let sexp_of_script (metadata, install_actions, uninstall_actions) =
    let f (action_id, install_action) =
      List [ sexp_of_action_id action_id; sexp_of_install_action install_action ]
    in
    List [ sexp_of_metadata metadata; sexp_of_list f install_actions;
    sexp_of_list sexp_of_uninstall_action uninstall_actions ]

  let sexp_of_package (script, results) =
    let f (action_id, result) =
      List [ sexp_of_action_id action_id; sexp_of_result result ]
    in
    List [ sexp_of_script script; sexp_of_list f results ]

  let sexp_of_db db = sexp_of_list sexp_of_package db
  let sexp_of_conf conf =
    List [ sexp_of_list sexp_of_predicate conf.preds ]

  let sexp_of_pkg pkg =
    List [
      sexp_of_metadata pkg.metadata;
      sexp_of_size pkg.size_compressed;
      Atom pkg.filename;
      sexp_of_option sexp_of_string pkg.signature;
      sexp_of_string_list pkg.files;
      sexp_of_string_list pkg.deps
    ]

  let sexp_of_repo repo =
    List [
      Atom repo.repo_target;
      sexp_of_list sexp_of_pkg repo.pkglist
    ]

  let sexp_of_sherpa_conf sherpa_conf =
    List [
      Atom sherpa_conf.mirror;
      Atom sherpa_conf.sherpa_version;
      Atom sherpa_conf.download_folder;
      Atom sherpa_conf.arch;
    ]

  let metadata = sexp_of_metadata
  let conf = sexp_of_conf
  let db = sexp_of_db
  let sherpa_conf= sexp_of_sherpa_conf
  let script = sexp_of_script
  let repo = sexp_of_repo
end

module To : sig
  val script : t -> script
  val conf : t -> conf
  val db : t -> db
  val metadata : t -> metadata
  val sherpa_conf : t -> sherpa_conf
  val repo : t -> repo
end = struct
  let date_of_sexp sexp =
    let f = int_of_sexp in
    match sexp with
    | Sexplib.Sexp.List [ year; month; day; hour; minute ] ->
        (f year, f month, f day, f hour, f minute)
    | _ -> of_sexp_error "data_of_sexp: list of 5 int atoms needed" sexp

  let status_of_sexp (sexp : Sexplib.Sexp.t) =
    match sexp with
    | List [ Atom "Alpha"; i ] -> Alpha (int_of_sexp i)
    | List [ Atom "Beta"; i ] -> Beta (int_of_sexp i)
    | List [ Atom "RC"; i ] -> RC (int_of_sexp i)
    | List [ Atom "Snapshot_date"; date ] -> Snapshot_date (date_of_sexp date)
    | List [ Atom "Snapshot_hash"; Atom hash ] -> Snapshot_hash hash
    | List [ Atom "Stable" ] -> Stable
    | _ -> of_sexp_error "status_of_sexp: wrong atom or wrong atom argument" sexp

  let version_of_sexp sexp =
    match sexp with
    | List [ version_components; status; build_number ] ->
        let version_components = list_of_sexp int_of_sexp version_components in
        (version_components, status_of_sexp status, int_of_sexp build_number)
    | List _ -> of_sexp_error
        "version_of_sexp: list must contain exactly three elements" sexp
    | Atom _ -> of_sexp_error "version_of_sexp: list needed" sexp

  let string_list_of_sexp sexp = list_of_sexp string_of_sexp sexp
  let action_id_of_sexp = string_of_sexp

  let install_action_of_sexp sexp =
    match sexp with
    | List [ Atom "AHK"; params ] -> AHK (string_list_of_sexp params)
    | List [ Atom "Exec"; argv ] -> Exec (string_list_of_sexp argv)
    | List [ Atom "Expand"; List [ Atom orig; Atom dest ] ] ->
        Expand (orig, dest)
    | List [ Atom "MKdir"; Atom dir ] -> MKdir dir
    | List [ Atom "SearchReplace"; List [ files; Atom search; Atom replace ] ] ->
        SearchReplace (string_list_of_sexp files, search, replace)
    | _ -> of_sexp_error
        "install_action_of_sexp: wrong list or wrong list argument" sexp

  let uninstall_action_of_sexp sexp =
    match sexp with
    | List [ Atom "RM"; Atom dir ] -> RM dir
    | List [ Atom "Reverse"; Atom action_id ] -> Reverse action_id
    | _ -> of_sexp_error
        "uninstall_action_of_sexp: wrong list or wrong list argument" sexp

  let result_of_sexp sexp =
    match sexp with
    | List [ Atom "Filelist"; files ] -> Filelist (string_list_of_sexp files)
    | Atom "NA" -> NA
    | _ -> of_sexp_error
        "result_of_sexp: wrong atom, wrong list or wrong list argument" sexp

  let predicate_of_sexp sexp =
    match sexp with
    | List [ Atom name; values ] -> name, string_list_of_sexp values
    | _ -> of_sexp_error
        "predicate_of_sexp: wrong list or wrong list argument" sexp

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

  let record_of_sexp_aux ~f_name ~f_sexp ~duplicates ~conv ~res =
    match !res with
    | None -> res := Some (conv f_sexp)
    | Some _ -> duplicates := f_name :: !duplicates

  let metadata_of_sexp sexp =
    let n = ref None in
    let name = n and size_expanded = ref None and version = ref None and
    packager_email = n and packager_name = n and description = n and host = n and
    target = n and predicates = ref None and comments = ref None in
    let duplicates = ref [] in
    let extra = ref [] in
    let rec aux = function
      | List [ Atom f_name; f_sexp ] :: q ->
          let f ~conv ~res =
            record_of_sexp_aux ~f_name ~f_sexp ~duplicates ~conv ~res
          in
          let g = function
            | List [ Atom s1; Atom s2 ] -> s1, s2
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

  let script_of_sexp sexp =
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

  let package_of_sexp sexp =
    let f = function
      | List [ action_id; result ] ->
          action_id_of_sexp action_id, result_of_sexp result
      | _ -> of_sexp_error "package_of_sexp: results: atom or wrong list" sexp
    in
    match sexp with
    | List [ script; results ] ->
        script_of_sexp script, list_of_sexp f results
    | _ -> of_sexp_error "package_of_sexp: atom or wrong list" sexp

  let db_of_sexp sexp = list_of_sexp package_of_sexp sexp

  let conf_of_sexp sexp =
    match sexp with
    | List [ predicates ] -> { preds = list_of_sexp predicate_of_sexp predicates }
    | _ -> of_sexp_error "conf_of_sexp: atom or wrong list" sexp

  let pkg_of_sexp sexp =
    let metadata = ref None and size_compressed = ref None and filename = ref None
    and signature = ref None and files = ref None and deps = ref None in
    let duplicates = ref [] in
    let extra = ref [] in
    let rec aux = function
      | List [ Atom f_name; f_sexp ] :: q ->
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

  let repo_of_sexp sexp =
    let repo_target = ref None and pkglist = ref None in
    let duplicates = ref [] in
    let extra = ref [] in
    let rec aux = function
      | List [ Atom f_name; f_sexp ] :: q ->
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

  let sherpa_conf_of_sexp sexp =
    let mirror = ref None and sherpa_version = ref None and download_folder =
      ref None and arch = ref None in
    let duplicates = ref [] in
    let extra = ref [] in
    let rec aux = function
      | List [ Atom f_name; f_sexp ] :: q ->
          let f ~conv ~res =
            record_of_sexp_aux ~f_name ~f_sexp ~duplicates ~conv ~res
          in
          (match f_name with
          | "mirror" -> f ~conv:string_of_sexp ~res:mirror
          | "sherpa_version" -> f ~conv:string_of_sexp ~res:sherpa_version
          | "download_folder" -> f ~conv:string_of_sexp ~res:download_folder
          | "arch" -> f ~conv:string_of_sexp ~res:arch
          | _ -> extra := f_name :: !extra);
          aux q
      | [] -> (
          match !mirror, !sherpa_version, !download_folder, !arch with
          | Some mirror, Some sherpa_version, Some download_folder, Some arch ->
              { mirror = mirror; sherpa_version =  sherpa_version; download_folder
              = download_folder; arch = arch }
          | _ -> 
              record_undefined_fields ~name:"sherpa_conf_of_sexp" ~sexp ~l:[
                "mirror", !mirror = None; "sherpa_version", !sherpa_version =
                None; "download_folder", !download_folder = None;
                "arch", !arch = None
              ]
        )
      | _ -> of_sexp_error "sherpa_conf_of_sexp: atom or wrong list element" sexp
    in
    match sexp with
    | List l -> aux l
    | _ -> of_sexp_error "sherpa_conf_of_sexp: atom argument" sexp

  let script = script_of_sexp
  let conf = conf_of_sexp
  let db = db_of_sexp
  let metadata = metadata_of_sexp
  let sherpa_conf = sherpa_conf_of_sexp
  let repo = repo_of_sexp
end

