module Radio : sig
  val grouped :
    box:Efl.Evas.obj -> w:Efl.Evas.obj -> string array -> unit -> string
end

module Table : sig
  val table :
    scroller:Efl.Evas.obj ->
      populate:(unit ->
        (int * (unit -> unit) * ((int -> Efl.Evas.obj -> unit) -> 'a)) option)
        -> Efl.Evas.obj -> Efl.Evas.obj
end
