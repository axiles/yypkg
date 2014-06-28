module Systems : sig
  val prompt_for_mirror : unit -> string
  val prompt :
    cb_ok:(system:string -> arch:string -> file:string -> string) -> unit
end
