type 'a t = ('a -> unit) ref

val create : string -> 'a t
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> unit
