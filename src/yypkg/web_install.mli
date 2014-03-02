val get_packages :
  conf:Types.conf -> follow:bool -> dest:FilePath.filename
  -> packages:string list -> string list
val cli_spec : Args.child
val main : start_dir:FilePath.filename -> Args.opt list -> unit
