exception Unknown_package of string
val read : unit -> Types.SherpaT.sherpa_conf
val update : (Types.SherpaT.sherpa_conf -> Types.SherpaT.sherpa_conf) -> unit
val get_uri : string -> string -> unit
val repo : conf:Types.SherpaT.sherpa_conf -> unit -> Types.SherpaT.repo
val pkglist :
  sherpa_conf:Types.SherpaT.sherpa_conf -> yypkg_conf:Types.conf
  -> Types.pkg list
val get_packages :
  yypkg_conf:Types.conf ->
  sherpa_conf:Types.SherpaT.sherpa_conf ->
  follow:bool ->
  dest:FilePath.filename -> packages:string list -> string list
