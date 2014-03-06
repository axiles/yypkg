type 'a t = (string * string list * 'a) list

val possible_values : t:'a t -> string
val to_string : t:'a t -> 'a -> string
val of_string : t:'a t -> string -> 'a

val bool : bool t
