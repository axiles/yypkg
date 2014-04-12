module Button : sig
  val abort : string
end

val main : unit -> unit
val msgbox : ?title:string -> buttons:string list -> string -> int
