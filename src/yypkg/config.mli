module Predicates : sig
  val set : Types.conf -> string * string list -> Types.conf
  val unset : Types.conf -> string -> Types.conf
end
val read : unit -> Types.conf
val update : (Types.conf -> Types.conf) -> Types.conf
val main : Args.opt list -> unit
val cli_spec : Args.child
