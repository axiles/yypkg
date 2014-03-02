module Predicates : sig
  val set : Types.conf -> string * string list -> Types.conf
  val unset : Types.conf -> string -> Types.conf
end
val read : unit -> Types.conf
val main : Args.opt list -> unit
val cli_spec : Args.child
