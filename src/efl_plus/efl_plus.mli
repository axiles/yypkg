module Radio : sig
  val grouped :
    box:Efl.Evas.obj -> w:Efl.Evas.obj -> string array -> unit -> string
end

module FileEntryButton : sig
  val build : box:Efl.Evas.obj -> w:Efl.Evas.obj -> unit -> string option
end
