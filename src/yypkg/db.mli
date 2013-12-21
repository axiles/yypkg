val install_package : Types.db -> Types.package -> Types.db
val uninstall_package : Types.db -> string -> Types.db
val read : unit -> Types.db
val update : (Types.db -> Types.db) -> unit
