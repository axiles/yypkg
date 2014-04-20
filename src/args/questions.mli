module Path : sig
  val get : ?mkdir:(string -> unit) -> existing:bool -> string
end
module Choice : sig
  val get : 'a StringMatcher.t -> 'a
end
module String : sig
  val get : unit -> string
end
