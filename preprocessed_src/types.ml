open Printf
  
(* NOTE: this has to be kept ordered !!! *)
type status =
  | Alpha of int | Beta of int | RC of int | Snapshot of string | Stable

type version =
  { major : int; minor : int; release : int; status : status;
    package_iteration : int
  }

(* both don't behave the same way of course *)
type tar_kind = | BSD | GNU

(* not really used right now, might well be dropped in the future
 * I think I've even forgotten why I wanted to have different types for them
 * (well, for safety of course, but what exactly ? *)
type absolute_path = string

let absolute_path_of_sexp__ =
  let _loc = "Types.absolute_path"
  in fun sexp -> Sexplib.Conv.string_of_sexp sexp
  
let absolute_path_of_sexp sexp =
  try absolute_path_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((msg, sexp)) ->
      Sexplib.Conv.of_sexp_error msg sexp
  
let sexp_of_absolute_path v = Sexplib.Conv.sexp_of_string v
  
type relative_path = string

let relative_path_of_sexp__ =
  let _loc = "Types.relative_path"
  in fun sexp -> Sexplib.Conv.string_of_sexp sexp
  
let relative_path_of_sexp sexp =
  try relative_path_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((msg, sexp)) ->
      Sexplib.Conv.of_sexp_error msg sexp
  
let sexp_of_relative_path v = Sexplib.Conv.sexp_of_string v
  
(* paths outside the package, on the hard drive *)
type outside_path = string

let outside_path_of_sexp__ =
  let _loc = "Types.outside_path"
  in fun sexp -> Sexplib.Conv.string_of_sexp sexp
  
let outside_path_of_sexp sexp =
  try outside_path_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((msg, sexp)) ->
      Sexplib.Conv.of_sexp_error msg sexp
  
let sexp_of_outside_path v = Sexplib.Conv.sexp_of_string v
  
(* paths inside the package *)
type inside_path = relative_path

let inside_path_of_sexp__ =
  let _loc = "Types.inside_path" in fun sexp -> relative_path_of_sexp sexp
  
let inside_path_of_sexp sexp =
  try inside_path_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((msg, sexp)) ->
      Sexplib.Conv.of_sexp_error msg sexp
  
let sexp_of_inside_path v = sexp_of_relative_path v
  
type param = string

let param_of_sexp__ =
  let _loc = "Types.param" in fun sexp -> Sexplib.Conv.string_of_sexp sexp
  
let param_of_sexp sexp =
  try param_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((msg, sexp)) ->
      Sexplib.Conv.of_sexp_error msg sexp
  
let sexp_of_param v = Sexplib.Conv.sexp_of_string v
  
type params = string list

let params_of_sexp__ =
  let _loc = "Types.params"
  in fun sexp -> Sexplib.Conv.list_of_sexp Sexplib.Conv.string_of_sexp sexp
  
let params_of_sexp sexp =
  try params_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((msg, sexp)) ->
      Sexplib.Conv.of_sexp_error msg sexp
  
let sexp_of_params v =
  Sexplib.Conv.sexp_of_list Sexplib.Conv.sexp_of_string v
  
type argv = string list

let argv_of_sexp__ =
  let _loc = "Types.argv"
  in fun sexp -> Sexplib.Conv.list_of_sexp Sexplib.Conv.string_of_sexp sexp
  
let argv_of_sexp sexp =
  try argv_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((msg, sexp)) ->
      Sexplib.Conv.of_sexp_error msg sexp
  
let sexp_of_argv v = Sexplib.Conv.sexp_of_list Sexplib.Conv.sexp_of_string v
  
(* this is only a name, an identifier *)
type action_id = string

let action_id_of_sexp__ =
  let _loc = "Types.action_id"
  in fun sexp -> Sexplib.Conv.string_of_sexp sexp
  
let action_id_of_sexp sexp =
  try action_id_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((msg, sexp)) ->
      Sexplib.Conv.of_sexp_error msg sexp
  
let sexp_of_action_id v = Sexplib.Conv.sexp_of_string v
  
type install_action =
  | AHK of params
  | Exec of argv
  | Expand of inside_path * outside_path
  | MKdir of outside_path

let install_action_of_sexp__ =
  let _loc = "Types.install_action"
  in
    function
    | (Sexplib.Sexp.List
         (Sexplib.Sexp.Atom (("aHK" | "AHK" as tag)) :: sexp_args)
       as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = params_of_sexp v1 in AHK v1
         | _ -> Sexplib.Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexplib.Sexp.List
         (Sexplib.Sexp.Atom (("exec" | "Exec" as tag)) :: sexp_args)
       as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = argv_of_sexp v1 in Exec v1
         | _ -> Sexplib.Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexplib.Sexp.List
         (Sexplib.Sexp.Atom (("expand" | "Expand" as tag)) :: sexp_args)
       as sexp) ->
        (match sexp_args with
         | [ v1; v2 ] ->
             let v1 = inside_path_of_sexp v1
             and v2 = outside_path_of_sexp v2
             in Expand ((v1, v2))
         | _ -> Sexplib.Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexplib.Sexp.List
         (Sexplib.Sexp.Atom (("mKdir" | "MKdir" as tag)) :: sexp_args)
       as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = outside_path_of_sexp v1 in MKdir v1
         | _ -> Sexplib.Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexplib.Sexp.Atom ("aHK" | "AHK") as sexp) ->
        Sexplib.Conv_error.stag_takes_args _loc sexp
    | (Sexplib.Sexp.Atom ("exec" | "Exec") as sexp) ->
        Sexplib.Conv_error.stag_takes_args _loc sexp
    | (Sexplib.Sexp.Atom ("expand" | "Expand") as sexp) ->
        Sexplib.Conv_error.stag_takes_args _loc sexp
    | (Sexplib.Sexp.Atom ("mKdir" | "MKdir") as sexp) ->
        Sexplib.Conv_error.stag_takes_args _loc sexp
    | (Sexplib.Sexp.List (Sexplib.Sexp.List _ :: _) as sexp) ->
        Sexplib.Conv_error.nested_list_invalid_sum _loc sexp
    | (Sexplib.Sexp.List [] as sexp) ->
        Sexplib.Conv_error.empty_list_invalid_sum _loc sexp
    | sexp -> Sexplib.Conv_error.unexpected_stag _loc sexp
  
let install_action_of_sexp sexp = install_action_of_sexp__ sexp
  
let sexp_of_install_action =
  function
  | AHK v1 ->
      let v1 = sexp_of_params v1
      in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "AHK"; v1 ]
  | Exec v1 ->
      let v1 = sexp_of_argv v1
      in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Exec"; v1 ]
  | Expand ((v1, v2)) ->
      let v1 = sexp_of_inside_path v1
      and v2 = sexp_of_outside_path v2
      in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Expand"; v1; v2 ]
  | MKdir v1 ->
      let v1 = sexp_of_outside_path v1
      in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "MKdir"; v1 ]
  
type uninstall_action = | RM of outside_path | Reverse of action_id

let uninstall_action_of_sexp__ =
  let _loc = "Types.uninstall_action"
  in
    function
    | (Sexplib.Sexp.List
         (Sexplib.Sexp.Atom (("rM" | "RM" as tag)) :: sexp_args)
       as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = outside_path_of_sexp v1 in RM v1
         | _ -> Sexplib.Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexplib.Sexp.List
         (Sexplib.Sexp.Atom (("reverse" | "Reverse" as tag)) :: sexp_args)
       as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = action_id_of_sexp v1 in Reverse v1
         | _ -> Sexplib.Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexplib.Sexp.Atom ("rM" | "RM") as sexp) ->
        Sexplib.Conv_error.stag_takes_args _loc sexp
    | (Sexplib.Sexp.Atom ("reverse" | "Reverse") as sexp) ->
        Sexplib.Conv_error.stag_takes_args _loc sexp
    | (Sexplib.Sexp.List (Sexplib.Sexp.List _ :: _) as sexp) ->
        Sexplib.Conv_error.nested_list_invalid_sum _loc sexp
    | (Sexplib.Sexp.List [] as sexp) ->
        Sexplib.Conv_error.empty_list_invalid_sum _loc sexp
    | sexp -> Sexplib.Conv_error.unexpected_stag _loc sexp
  
let uninstall_action_of_sexp sexp = uninstall_action_of_sexp__ sexp
  
let sexp_of_uninstall_action =
  function
  | RM v1 ->
      let v1 = sexp_of_outside_path v1
      in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "RM"; v1 ]
  | Reverse v1 ->
      let v1 = sexp_of_action_id v1
      in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Reverse"; v1 ]
  
type results = | Filelist of string list | NA

let results_of_sexp__ =
  let _loc = "Types.results"
  in
    function
    | (Sexplib.Sexp.List
         (Sexplib.Sexp.Atom (("filelist" | "Filelist" as tag)) :: sexp_args)
       as sexp) ->
        (match sexp_args with
         | [ v1 ] ->
             let v1 =
               Sexplib.Conv.list_of_sexp Sexplib.Conv.string_of_sexp v1
             in Filelist v1
         | _ -> Sexplib.Conv_error.stag_incorrect_n_args _loc tag sexp)
    | Sexplib.Sexp.Atom ("nA" | "NA") -> NA
    | (Sexplib.Sexp.Atom ("filelist" | "Filelist") as sexp) ->
        Sexplib.Conv_error.stag_takes_args _loc sexp
    | (Sexplib.Sexp.List (Sexplib.Sexp.Atom ("nA" | "NA") :: _) as sexp) ->
        Sexplib.Conv_error.stag_no_args _loc sexp
    | (Sexplib.Sexp.List (Sexplib.Sexp.List _ :: _) as sexp) ->
        Sexplib.Conv_error.nested_list_invalid_sum _loc sexp
    | (Sexplib.Sexp.List [] as sexp) ->
        Sexplib.Conv_error.empty_list_invalid_sum _loc sexp
    | sexp -> Sexplib.Conv_error.unexpected_stag _loc sexp
  
let results_of_sexp sexp = results_of_sexp__ sexp
  
let sexp_of_results =
  function
  | Filelist v1 ->
      let v1 = Sexplib.Conv.sexp_of_list Sexplib.Conv.sexp_of_string v1
      in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Filelist"; v1 ]
  | NA -> Sexplib.Sexp.Atom "NA"
  
type predicate = (string * (string list))

let predicate_of_sexp__ =
  let _loc = "Types.predicate"
  in
    function
    | Sexplib.Sexp.List ([ v1; v2 ]) ->
        let v1 = Sexplib.Conv.string_of_sexp v1
        and v2 = Sexplib.Conv.list_of_sexp Sexplib.Conv.string_of_sexp v2
        in (v1, v2)
    | sexp -> Sexplib.Conv_error.tuple_of_size_n_expected _loc 2 sexp
  
let predicate_of_sexp sexp =
  try predicate_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((msg, sexp)) ->
      Sexplib.Conv.of_sexp_error msg sexp
  
let sexp_of_predicate (v1, v2) =
  let v1 = Sexplib.Conv.sexp_of_string v1
  and v2 = Sexplib.Conv.sexp_of_list Sexplib.Conv.sexp_of_string v2
  in Sexplib.Sexp.List [ v1; v2 ]
  
type predicates = predicate list

let predicates_of_sexp__ =
  let _loc = "Types.predicates"
  in fun sexp -> Sexplib.Conv.list_of_sexp predicate_of_sexp sexp
  
let predicates_of_sexp sexp =
  try predicates_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((msg, sexp)) ->
      Sexplib.Conv.of_sexp_error msg sexp
  
let sexp_of_predicates v = Sexplib.Conv.sexp_of_list sexp_of_predicate v
  
exception Unmatched_predicates of (string * string) list
  
type metadata =
  { package_name : string; package_size_expanded : string;
    package_version : string; packager_email : string;
    packager_name : string; description : string;
    predicates : (string * string) list; comments : string
  }

let metadata_of_sexp__ =
  let _loc = "Types.metadata"
  in
    function
    | (Sexplib.Sexp.List field_sexps as sexp) ->
        let package_name_field = ref None
        and package_size_expanded_field = ref None
        and package_version_field = ref None
        and packager_email_field = ref None
        and packager_name_field = ref None and description_field = ref None
        and predicates_field = ref None and comments_field = ref None
        and duplicates = ref [] and extra = ref [] in
        let rec iter =
          (function
           | Sexplib.Sexp.List ([ Sexplib.Sexp.Atom field_name; field_sexp ]) ::
               tail ->
               ((match field_name with
                 | "package_name" ->
                     (match !package_name_field with
                      | None ->
                          let fvalue = Sexplib.Conv.string_of_sexp field_sexp
                          in package_name_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "package_size_expanded" ->
                     (match !package_size_expanded_field with
                      | None ->
                          let fvalue = Sexplib.Conv.string_of_sexp field_sexp
                          in package_size_expanded_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "package_version" ->
                     (match !package_version_field with
                      | None ->
                          let fvalue = Sexplib.Conv.string_of_sexp field_sexp
                          in package_version_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "packager_email" ->
                     (match !packager_email_field with
                      | None ->
                          let fvalue = Sexplib.Conv.string_of_sexp field_sexp
                          in packager_email_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "packager_name" ->
                     (match !packager_name_field with
                      | None ->
                          let fvalue = Sexplib.Conv.string_of_sexp field_sexp
                          in packager_name_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "description" ->
                     (match !description_field with
                      | None ->
                          let fvalue = Sexplib.Conv.string_of_sexp field_sexp
                          in description_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "predicates" ->
                     (match !predicates_field with
                      | None ->
                          let fvalue =
                            Sexplib.Conv.list_of_sexp
                              (function
                               | Sexplib.Sexp.List ([ v1; v2 ]) ->
                                   let v1 = Sexplib.Conv.string_of_sexp v1
                                   and v2 = Sexplib.Conv.string_of_sexp v2
                                   in (v1, v2)
                               | sexp ->
                                   Sexplib.Conv_error.
                                     tuple_of_size_n_expected _loc 2 sexp)
                              field_sexp
                          in predicates_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "comments" ->
                     (match !comments_field with
                      | None ->
                          let fvalue = Sexplib.Conv.string_of_sexp field_sexp
                          in comments_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | _ ->
                     if !Sexplib.Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | sexp :: _ ->
               Sexplib.Conv_error.record_only_pairs_expected _loc sexp
           | [] -> ())
        in
          (iter field_sexps;
           if Pervasives.( <> ) !duplicates []
           then
             Sexplib.Conv_error.record_duplicate_fields _loc !duplicates sexp
           else
             if Pervasives.( <> ) !extra []
             then Sexplib.Conv_error.record_extra_fields _loc !extra sexp
             else
               (match ((!package_name_field), (!package_size_expanded_field),
                       (!package_version_field), (!packager_email_field),
                       (!packager_name_field), (!description_field),
                       (!predicates_field), (!comments_field))
                with
                | (Some package_name_value, Some package_size_expanded_value,
                   Some package_version_value, Some packager_email_value,
                   Some packager_name_value, Some description_value,
                   Some predicates_value, Some comments_value) ->
                    {
                      package_name = package_name_value;
                      package_size_expanded = package_size_expanded_value;
                      package_version = package_version_value;
                      packager_email = packager_email_value;
                      packager_name = packager_name_value;
                      description = description_value;
                      predicates = predicates_value;
                      comments = comments_value;
                    }
                | _ ->
                    Sexplib.Conv_error.record_undefined_elements _loc sexp
                      [ ((Pervasives.( = ) !package_name_field None),
                         "package_name");
                        ((Pervasives.( = ) !package_size_expanded_field None),
                         "package_size_expanded");
                        ((Pervasives.( = ) !package_version_field None),
                         "package_version");
                        ((Pervasives.( = ) !packager_email_field None),
                         "packager_email");
                        ((Pervasives.( = ) !packager_name_field None),
                         "packager_name");
                        ((Pervasives.( = ) !description_field None),
                         "description");
                        ((Pervasives.( = ) !predicates_field None),
                         "predicates");
                        ((Pervasives.( = ) !comments_field None), "comments") ]))
    | (Sexplib.Sexp.Atom _ as sexp) ->
        Sexplib.Conv_error.record_list_instead_atom _loc sexp
  
let metadata_of_sexp sexp = metadata_of_sexp__ sexp
  
let sexp_of_metadata {
                       package_name = v_package_name;
                       package_size_expanded = v_package_size_expanded;
                       package_version = v_package_version;
                       packager_email = v_packager_email;
                       packager_name = v_packager_name;
                       description = v_description;
                       predicates = v_predicates;
                       comments = v_comments
                     } =
  let bnds = [] in
  let arg = Sexplib.Conv.sexp_of_string v_comments in
  let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "comments"; arg ] in
  let bnds = bnd :: bnds in
  let arg =
    Sexplib.Conv.sexp_of_list
      (fun (v1, v2) ->
         let v1 = Sexplib.Conv.sexp_of_string v1
         and v2 = Sexplib.Conv.sexp_of_string v2
         in Sexplib.Sexp.List [ v1; v2 ])
      v_predicates in
  let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "predicates"; arg ] in
  let bnds = bnd :: bnds in
  let arg = Sexplib.Conv.sexp_of_string v_description in
  let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "description"; arg ] in
  let bnds = bnd :: bnds in
  let arg = Sexplib.Conv.sexp_of_string v_packager_name in
  let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "packager_name"; arg ] in
  let bnds = bnd :: bnds in
  let arg = Sexplib.Conv.sexp_of_string v_packager_email in
  let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "packager_email"; arg ] in
  let bnds = bnd :: bnds in
  let arg = Sexplib.Conv.sexp_of_string v_package_version in
  let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "package_version"; arg ] in
  let bnds = bnd :: bnds in
  let arg = Sexplib.Conv.sexp_of_string v_package_size_expanded in
  let bnd =
    Sexplib.Sexp.List [ Sexplib.Sexp.Atom "package_size_expanded"; arg ] in
  let bnds = bnd :: bnds in
  let arg = Sexplib.Conv.sexp_of_string v_package_name in
  let bnd = Sexplib.Sexp.List [ Sexplib.Sexp.Atom "package_name"; arg ] in
  let bnds = bnd :: bnds in Sexplib.Sexp.List bnds
  
type script =
  (metadata * ((action_id * install_action) list) * (uninstall_action list))

let script_of_sexp__ =
  let _loc = "Types.script"
  in
    function
    | Sexplib.Sexp.List ([ v1; v2; v3 ]) ->
        let v1 = metadata_of_sexp v1
        and v2 =
          Sexplib.Conv.list_of_sexp
            (function
             | Sexplib.Sexp.List ([ v1; v2 ]) ->
                 let v1 = action_id_of_sexp v1
                 and v2 = install_action_of_sexp v2
                 in (v1, v2)
             | sexp ->
                 Sexplib.Conv_error.tuple_of_size_n_expected _loc 2 sexp)
            v2
        and v3 = Sexplib.Conv.list_of_sexp uninstall_action_of_sexp v3
        in (v1, v2, v3)
    | sexp -> Sexplib.Conv_error.tuple_of_size_n_expected _loc 3 sexp
  
let script_of_sexp sexp =
  try script_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((msg, sexp)) ->
      Sexplib.Conv.of_sexp_error msg sexp
  
let sexp_of_script (v1, v2, v3) =
  let v1 = sexp_of_metadata v1
  and v2 =
    Sexplib.Conv.sexp_of_list
      (fun (v1, v2) ->
         let v1 = sexp_of_action_id v1
         and v2 = sexp_of_install_action v2
         in Sexplib.Sexp.List [ v1; v2 ])
      v2
  and v3 = Sexplib.Conv.sexp_of_list sexp_of_uninstall_action v3
  in Sexplib.Sexp.List [ v1; v2; v3 ]
  
type package = (script * ((action_id * results) list))

let package_of_sexp__ =
  let _loc = "Types.package"
  in
    function
    | Sexplib.Sexp.List ([ v1; v2 ]) ->
        let v1 = script_of_sexp v1
        and v2 =
          Sexplib.Conv.list_of_sexp
            (function
             | Sexplib.Sexp.List ([ v1; v2 ]) ->
                 let v1 = action_id_of_sexp v1
                 and v2 = results_of_sexp v2
                 in (v1, v2)
             | sexp ->
                 Sexplib.Conv_error.tuple_of_size_n_expected _loc 2 sexp)
            v2
        in (v1, v2)
    | sexp -> Sexplib.Conv_error.tuple_of_size_n_expected _loc 2 sexp
  
let package_of_sexp sexp =
  try package_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((msg, sexp)) ->
      Sexplib.Conv.of_sexp_error msg sexp
  
let sexp_of_package (v1, v2) =
  let v1 = sexp_of_script v1
  and v2 =
    Sexplib.Conv.sexp_of_list
      (fun (v1, v2) ->
         let v1 = sexp_of_action_id v1
         and v2 = sexp_of_results v2
         in Sexplib.Sexp.List [ v1; v2 ])
      v2
  in Sexplib.Sexp.List [ v1; v2 ]
  
type db = package list

let db_of_sexp__ =
  let _loc = "Types.db"
  in fun sexp -> Sexplib.Conv.list_of_sexp package_of_sexp sexp
  
let db_of_sexp sexp =
  try db_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((msg, sexp)) ->
      Sexplib.Conv.of_sexp_error msg sexp
  
let sexp_of_db v = Sexplib.Conv.sexp_of_list sexp_of_package v
  
(* list of predicates that are checked before installing apackage: for instance:
  * arch=x86_64,noarch
  * stability=stable,release_candidate *)
type conf = predicates

let conf_of_sexp__ =
  let _loc = "Types.conf" in fun sexp -> predicates_of_sexp sexp
  
let conf_of_sexp sexp =
  try conf_of_sexp__ sexp
  with
  | Sexplib.Conv_error.No_variant_match ((msg, sexp)) ->
      Sexplib.Conv.of_sexp_error msg sexp
  
let sexp_of_conf v = sexp_of_predicates v
  

