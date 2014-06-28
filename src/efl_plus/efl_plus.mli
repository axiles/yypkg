module Radio : sig
  val grouped :
    box:Efl.Evas.obj -> w:Efl.Evas.obj -> string array -> unit -> string
end

module Table : sig
  type t = private {
    table : Efl.Evas_object.t;
    bg_reset_even : Efl.Evas_object.t -> unit;
    bg_reset_odd : Efl.Evas_object.t -> unit;
    q_selected : ((Efl.Evas_object.t -> unit) * Efl.Evas_object.t) Queue.t;
  }

  val add_rows :
    populate:(unit ->
      (int * (unit -> unit) * ((int -> Efl.Evas.obj -> unit) -> 'a)) option)
      -> w:Efl.Evas.obj -> t:t -> 'b -> int

  val table :
    scroller:Efl.Evas.obj ->
    populate:(unit ->
      (int * (unit -> unit) * ((int -> Efl.Evas.obj -> unit) -> 'a)) option)
      -> Efl.Evas.obj -> t
end
