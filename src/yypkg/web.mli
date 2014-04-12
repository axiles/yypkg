val packages :
  conf:Types.conf -> follow:bool -> wishes:string list -> Types.pkg list
val download :
  conf:Types.conf -> dest:FilePath.filename -> Types.pkg list -> string list
val needs_update :
  db:Types.package list -> Types.pkg -> bool
val cli_spec : Args.child
val main : start_dir:FilePath.filename -> Args.opt list -> unit