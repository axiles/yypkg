val set : Types.conf -> string * string list -> Types.conf
val unset : Types.conf -> string -> Types.conf
val read : unit -> Types.conf
val update : (Types.conf -> Types.conf) -> unit
val print_predicates : Types.conf -> unit
