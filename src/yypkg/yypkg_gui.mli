module Systems : sig
  val prompt :
    cb_ok:(system:string -> arch:string -> file:string option -> bool) -> unit
end
