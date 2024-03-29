val packages :
  conf:Types.conf -> follow:bool -> wishes:string list -> Types.Repo.pkg list

val download_init : conf:Types.conf -> dest:FilePath.filename -> string

val download_one :
  conf:Types.conf
  -> dest:FilePath.filename
  -> agent:string
  -> ?progress:(Types.Repo.pkg
        -> (string:string -> offset:int -> length:int -> unit) * (unit -> unit))
  -> Types.Repo.pkg
  -> string

val download :
  conf:Types.conf -> dest:FilePath.filename
  -> ?progress:(Types.Repo.pkg
        -> (string:string -> offset:int -> length:int -> unit) * (unit -> unit))
  -> Types.Repo.pkg list
  -> string list
val needs_update : db:Types.package list -> Types.Repo.pkg -> bool
val cli_spec : Args.child
val main : start_dir:FilePath.filename -> Args.opt list -> unit
