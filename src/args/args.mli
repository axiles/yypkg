type child = { name : string; children : child list; help : string; }
and spec = child list
type opt = Val of string | Opt of (string * opt list)
val spec : name:string -> children:child list -> help:string -> child
exception Option_specification_is_ambiguous
exception Incomplete_parsing of (opt list * string list)
exception Parsing_failed of string
val bprint_spec : Buffer.t -> int -> child -> unit
val parse : child list -> string array -> opt list
val wants_help : unit -> bool
val nothing_given : unit -> bool
val usage_msg : child list -> string -> child
val is_opt : ?s:string -> opt -> bool
val is_val : opt -> bool
val to_string_list : opt list -> string list
