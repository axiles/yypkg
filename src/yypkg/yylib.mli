exception Unknown_package of string
external remove : string -> unit = "yy_remove"
external create_reparse_point : string -> string -> unit
  = "create_reparse_point"
val conf_dir : string
val db_dir : string
val db_path : string
val default_download_path : string
val conf_path : string
val expand_environment_variables : string -> string
val command : string list -> string list
val mkdir : FilePath.filename -> FilePath.filename
val expand : string -> string -> FilePath.filename -> string list
val rm : FilePath.DefaultPath.filename -> unit
val file_exists_in_package : string -> Types.package -> bool
val metadata_of_pkg : Types.package -> Types.metadata
val metadata_of_script : Types.script -> Types.metadata
val package_is_named : string -> Types.package -> bool
val find_all_by_name_regex : Types.db -> Str.regexp -> Types.db
val find_all_by_name :
  pkglist:Types.Repo.pkg list -> name_list:string list -> Types.Repo.pkg list
val is_installed : Types.db -> string -> bool
val sanity_checks : string -> unit
val symlink :
  target:FilePath.filename -> name:FilePath.filename ->
  kind:Types.filekind -> unit
val predicate_holds : Types.predicate list -> string * string -> bool
val xz_opt : int64 -> string
val tar_xz : tar_args:(string array list) -> xz_opt:string -> out:string -> unit

(* val get_packages :
  conf:Types.conf -> follow:bool ->
  dest:FilePath.filename -> packages:string list -> string list *)
