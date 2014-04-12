type child = { name : string; children : child list; help : string; }
and spec = child list
type opt = Val of string | Opt of (string * opt list)
val spec : name:string -> children:child list -> help:string -> child
exception Option_specification_is_ambiguous
exception Incomplete_parsing of (opt list * string list)
exception Parsing_failed of string
val bprint_spec : Buffer.t -> int -> child -> unit
val parse : child list -> string array -> opt list
val nothing_given : unit -> bool
val wants_help : unit -> bool
val usage_msg : child list -> string -> child
val is_opt : ?s:string -> opt -> bool
val to_string_list : opt list -> string list
val fold_values :
  where:string -> init:'a ->
  (string * (accu:'a -> string -> opt option -> 'a)) list -> opt list -> 'a

module Get : sig
  val of_stringmatcher : 'a StringMatcher.t -> string -> opt option -> 'a
  val bool : string -> opt option -> bool
  val string : string -> opt option -> string
end
