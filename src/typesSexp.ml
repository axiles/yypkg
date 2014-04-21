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

open Pre_sexp
open Types

module Of : sig
  val metadata : metadata -> t
  val conf : conf -> t
  val db : db -> t
  val script : script -> t
  val repository : Types.Repo.t -> t
end = struct

  let sexp_of_int n = Atom (string_of_int n)
  let sexp_of_list sexp_of__a lst =
    List (List.rev (List.rev_map sexp_of__a lst))
  let sexp_of_string str = Atom str
  let sexp_of_int64 n = Atom (Int64.to_string n)
  let sexp_of_option sexp_of__a = function
    | Some x -> List [ Atom "some"; sexp_of__a x ]
    | None -> Atom "none"

  let sexp_of_version (source_version, build_number) =
    List [ Atom source_version; sexp_of_int build_number ]

  let sexp_of_string_list params = sexp_of_list sexp_of_string params

  let sexp_of_action_id s = Atom s

  let sexp_of_filekind = function
    | `Directory -> Atom "Directory"
    | `Unhandled reason -> List [ Atom "Unhandled"; Atom reason ]
    | `File -> Atom "File"

  let sexp_of_install_action install_action =
    match install_action with
    | Exec argv -> List [ Atom "Exec"; sexp_of_string_list argv ]
    | Expand (orig, dest) -> List [ Atom "Expand"; Atom orig; Atom dest ]
    | MKdir dir -> List [ Atom "Exec"; Atom dir ]
    | SearchReplace (file, search, replace) -> List [
        Atom "SearchReplace"; List [ Atom file; Atom search; Atom replace ]
      ]
    | Symlink (target, name, kind) -> List [
        Atom "Symlink"; List [ Atom target; Atom name; sexp_of_filekind kind ]
    ]

  let sexp_of_uninstall_action uninstall_action =
    match uninstall_action with
    | RM dir -> List [ Atom "RM"; Atom dir ]
    | Reverse action_id -> List [ Atom "Reverse"; Atom action_id ]

  let sexp_of_result = sexp_of_string_list

  let sexp_of_predicate (name, values) =
    List [ Atom name; sexp_of_string_list values ]

  let sexp_of_size size =
    let p s i =
      List [ Atom s; sexp_of_int64 i ]
    in
    match size with
    | FileUtil.TB i -> p "TB" i
    | FileUtil.GB i -> p "GB" i
    | FileUtil.MB i -> p "MB" i
    | FileUtil.KB i -> p "KB" i
    | FileUtil.B  i -> p  "B" i

  let sexp_of_string_tuple (s1, s2) =
    List [ Atom s1; Atom s2 ]

  let sexp_of_metadata m =
    List [
      List [ Atom "name"; Atom m.name ];
      List [ Atom "size_expanded"; sexp_of_size m.size_expanded ];
      List [ Atom "version"; sexp_of_version m.version ];
      List [ Atom "packager_email"; Atom m.packager_email ];
      List [ Atom "packager_name"; Atom m.packager_name ];
      List [ Atom "description"; Atom m.description ];
      List [ Atom "host"; Atom m.host ];
      List [ Atom "target"; sexp_of_option sexp_of_string m.target ];
      List [ Atom "predicates"; sexp_of_list sexp_of_string_tuple m.predicates ];
      List [ Atom "comments"; sexp_of_string_list m.comments ];
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
    List [
      List [ Atom "preds"; sexp_of_list sexp_of_predicate conf.preds ];
      List [ Atom "mirror"; sexp_of_string conf.mirror ];
    ]

  let sexp_of_pkg pkg =
    let open Types.Repo in
    List [
      List [ Atom "metadata"; sexp_of_metadata pkg.metadata ];
      List [ Atom "size_compressed"; sexp_of_size pkg.size_compressed ];
      List [ Atom "filename"; Atom pkg.filename ];
      List [ Atom "signature"; sexp_of_option sexp_of_string pkg.signature ];
      List [ Atom "files"; sexp_of_string_list pkg.files ];
      List [ Atom "deps"; sexp_of_string_list pkg.deps ];
      List [ Atom "sha3"; sexp_of_string pkg.sha3 ];
    ]

  let sexp_of_repository repository =
    List [
      List [ Atom "target"; Atom repository.Repo.target ];
      List [ Atom "host"; Atom repository.Repo.host ];
      List [ Atom "pkglist"; sexp_of_list sexp_of_pkg repository.Repo.pkglist ]
    ]

  let metadata = sexp_of_metadata
  let conf = sexp_of_conf
  let db = sexp_of_db
  let script = sexp_of_script
  let repository = sexp_of_repository
end

module To : sig
  val script : t -> script
  val conf : t -> conf
  val db : t -> db
  val metadata : t -> metadata
  val repository : t -> Types.Repo.t
end = struct
  module Conv : sig
    val of_sexp_error : string -> Pre_sexp.t -> 'a
    val int_of_sexp : Pre_sexp.t -> int
    val int64_of_sexp : Pre_sexp.t -> int64
    val list_of_sexp : (Pre_sexp.t -> 'a) -> Pre_sexp.t -> 'a list
    val string_of_sexp : Pre_sexp.t -> string
    val option_of_sexp : (Pre_sexp.t -> 'a) -> Pre_sexp.t -> 'a option
  end = struct
    let of_sexp_error what sexp =
      raise (Pre_sexp.Of_sexp_error (Failure what, sexp))

    let ints_of_sexp conv name sexp =
      match sexp with
      | Atom str ->
          (try conv str
          with exc -> of_sexp_error (name ^ ": " ^ Printexc.to_string exc) sexp)
      | List _ -> of_sexp_error (name ^ ": atom needed") sexp

    let int_of_sexp sexp = ints_of_sexp int_of_string "int_of_string" sexp

    let int64_of_sexp sexp = ints_of_sexp Int64.of_string "int64_of_string" sexp

    let list_of_sexp a__of_sexp sexp = match sexp with
      | List lst -> List.rev (List.rev_map a__of_sexp lst)
      | Atom _ -> of_sexp_error "list_of_sexp: list needed" sexp

    let string_of_sexp sexp = match sexp with
      | Atom str -> str
      | List _ -> of_sexp_error "string_of_sexp: atom needed" sexp

    let option_of_sexp a__of_sexp sexp =
      match sexp with
      | List [] | Atom ("none" | "None") -> None
      | List [el] | List [Atom ("some" | "Some"); el] -> Some (a__of_sexp el)
      | List _ ->
          of_sexp_error "option_of_sexp: list must represent optional value" sexp
      | Atom _ -> of_sexp_error "option_of_sexp: only none can be atom" sexp
  end

  open Conv

  module Record = struct
    let accumulate ~f_name ~f_sexp ~duplicates ~fields ~extra =
      try
        let is_set, conv = List.assoc f_name fields in
        if is_set () then
          duplicates := f_name :: !duplicates
        else
          conv f_sexp
      with
      | Not_found -> extra := f_name :: !extra
      | e -> Printf.eprintf "Unknown error with field `%s'.\n" f_name; raise e

    let field_iter ~func ~fields l =
      let extra = ref [] in
      let duplicates = ref [] in
      ListLabels.iter l ~f:(function
        | List [ Atom f_name; f_sexp ] ->
            accumulate ~f_name ~f_sexp ~duplicates ~fields ~extra
        | sexp ->
            of_sexp_error (func ^ ": atom or wrong list element") sexp
      );
      !duplicates, !extra

    let undefined_fields ~func ~fields ~sexp =
      let f errs (name, (is_set, _)) = if is_set () then name :: errs else errs in
      let msg = Printf.sprintf "%s: some fields are missing: %s" func
        (String.concat ", " (List.fold_left f [] fields))
      in
      of_sexp_error msg sexp

    let parse ~func ~sexp ~fields ~build_value =
      match sexp with
      | List l ->
          let _duplicates, _extra = field_iter ~func ~fields l in
          build_value ()
      | _ -> of_sexp_error (func ^ ": atom argument") sexp

    let field_spec name var conv =
      name, ((fun () -> !var != None) , (fun sexp -> var := Some (conv sexp)))
  end

  let version_of_sexp sexp =
    match sexp with
    | List [ source_version; build_number ] ->
        string_of_sexp source_version, int_of_sexp build_number
    | List _ -> of_sexp_error
        "version_of_sexp: list must contain exactly two elements" sexp
    | Atom _ -> of_sexp_error "version_of_sexp: list needed" sexp

  let string_list_of_sexp sexp = list_of_sexp string_of_sexp sexp
  let action_id_of_sexp = string_of_sexp

  let filekind_of_sexp sexp =
    match sexp with
    | Atom "File" -> `File
    | Atom "Directory" -> `Directory
    | List [ Atom "Unhandled"; Atom reason ] -> `Unhandled reason
    | _ ->
        of_sexp_error "filekind_of_sexp: list or wrong atom argument" sexp

  let install_action_of_sexp sexp =
    match sexp with
    | List [ Atom "Exec"; argv ] -> Exec (string_list_of_sexp argv)
    | List [ Atom "Expand"; Atom orig; Atom dest ] -> Expand (orig, dest)
    | List [ Atom "MKdir"; Atom dir ] -> MKdir dir
    | List [ Atom "SearchReplace"; List [Atom f; Atom search; Atom replace] ] ->
        SearchReplace (f, search, replace)
    | List [ Atom "Symlink"; List [ Atom target; Atom name; kind ] ] ->
        Symlink (target, name, filekind_of_sexp kind)
    | _ -> of_sexp_error
        "install_action_of_sexp: atom or wrong list argument" sexp

  let uninstall_action_of_sexp sexp =
    match sexp with
    | List [ Atom "RM"; Atom dir ] -> RM dir
    | List [ Atom "Reverse"; Atom action_id ] -> Reverse action_id
    | _ -> of_sexp_error
        "uninstall_action_of_sexp: atom or wrong list argument" sexp

  let result_of_sexp = string_list_of_sexp

  let predicate_of_sexp sexp =
    match sexp with
    | List [ Atom name; values ] -> name, string_list_of_sexp values
    | _ -> of_sexp_error
        "predicate_of_sexp: atom or wrong list argument" sexp

  let size_of_sexp sexp = match sexp with
    | List [ Atom mult; i ] ->
        let i = int64_of_sexp i in
        begin match mult with
        | "TB" -> FileUtil.TB i | "GB" -> FileUtil.GB i | "MB" -> FileUtil.MB i
        | "KB" -> FileUtil.KB i | "B" -> FileUtil.B i
        | _ -> of_sexp_error "size_of_sexp: invalid size prefix" sexp
        end
    | _ -> of_sexp_error "size_of_sexp: not a FileUtil.size" sexp

  let string_tuple_of_sexp = function
    | List [ Atom s1; Atom s2 ] -> s1, s2
    | sexp -> of_sexp_error "string_tuple_of_sexp: not a tuple" sexp

  let metadata_of_sexp sexp =
    let func = "metadata_of_sexp" in
    let name = ref None and size_expanded = ref None and version = ref None and
    packager_email = ref None and packager_name = ref None and description = ref
    None and host = ref None and target = ref None and predicates = ref None and
    comments = ref None in
    let fields = Record.([
      field_spec "name" name string_of_sexp;
      field_spec "size_expanded" size_expanded size_of_sexp;
      field_spec "version" version version_of_sexp;
      field_spec "packager_email" packager_email string_of_sexp;
      field_spec "packager_name" packager_name string_of_sexp;
      field_spec "description" description string_of_sexp;
      field_spec "host" host string_of_sexp;
      field_spec "target" target (option_of_sexp string_of_sexp);
      field_spec "predicates" predicates (list_of_sexp string_tuple_of_sexp);
      field_spec "comments" comments string_list_of_sexp;
    ])
    in
    let build_value () =
      match (!name, !size_expanded, !version, !packager_email, !packager_name,
      !description, !host, !target, !predicates, !comments) with
      | Some name, Some size_expanded, Some version, Some packager_email,
        Some packager_name, Some description, Some host, Some target,
        Some predicates, Some comments ->
          { name; size_expanded; version; packager_email; packager_name;
            description; host; target; predicates; comments }
      | _ ->
          Record.undefined_fields ~func ~sexp ~fields
    in
    Record.parse ~func ~sexp ~fields ~build_value

  let script_of_sexp sexp =
    let f = function
      | List [ action_id; install_action ] ->
          action_id_of_sexp action_id, install_action_of_sexp install_action
      | _ -> of_sexp_error
          "install_actions: atom or wrong list" sexp
    in
    match sexp with
    | List [ metadata; install_actions; uninstall_actions ] ->
        metadata_of_sexp metadata, list_of_sexp f install_actions, list_of_sexp
        uninstall_action_of_sexp uninstall_actions
    | _ -> of_sexp_error "script_of_sexp: atom or wrong list" sexp

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
    let func = "conf_of_sexp" in
    let preds = ref None and mirror = ref None in
    let fields = Record.([
      field_spec "preds" preds (list_of_sexp predicate_of_sexp);
      field_spec "mirror" mirror string_of_sexp;
    ])
    in
    let build_value () =
      match (!preds, !mirror) with
      | Some preds, Some mirror -> { preds; mirror }
      | _ -> Record.undefined_fields ~func ~sexp ~fields
    in
    Record.parse ~func ~sexp ~fields ~build_value

  let pkg_of_sexp sexp =
    let func = "pkg_of_sexp" in
    let metadata = ref None and size_compressed = ref None and files = ref None
    and filename = ref None and signature = ref None and deps = ref None
    and sha3 = ref None in
    let fields = Record.([
      field_spec "metadata" metadata metadata_of_sexp;
      field_spec "size_compressed" size_compressed size_of_sexp;
      field_spec "filename" filename string_of_sexp;
      field_spec "signature" signature (option_of_sexp string_of_sexp);
      field_spec "files" files string_list_of_sexp;
      field_spec "deps" deps string_list_of_sexp;
      field_spec "sha3" sha3 string_of_sexp;
    ])
    in
    let build_value () =
      match !metadata, !size_compressed, !filename, !signature, !files, !deps, !sha3
      with
      | Some metadata, Some size_compressed, Some filename, Some signature,
        Some files, Some deps, Some sha3 ->
          let open Types.Repo in
          { metadata; size_compressed; filename; signature; files; deps; sha3 }
      | _ ->
          Record.undefined_fields ~func ~sexp ~fields
    in
    Record.parse ~func ~sexp ~fields ~build_value

  let repository_of_sexp sexp =
    let func = "repository_of_sexp" in
    let target = ref None and host = ref None and pkglist = ref None in
    let fields = Record.([
      field_spec "target" target string_of_sexp;
      field_spec "host" host string_of_sexp;
      field_spec "pkglist" pkglist (list_of_sexp pkg_of_sexp);
    ])
    in
    let build_value () =
      match !target, !host, !pkglist with
      | Some target, Some host, Some pkglist ->
          { Types.Repo.target; host; pkglist }
      | _ -> 
          Record.undefined_fields ~func ~sexp ~fields
    in
    Record.parse ~func ~sexp ~fields ~build_value

  let script = script_of_sexp
  let conf = conf_of_sexp
  let db = db_of_sexp
  let metadata = metadata_of_sexp
  let repository = repository_of_sexp
end

