module HTTP_ : sig
  module Response : sig
    exception Client_Error of HTTP.Status.Client_Error.t
    exception Server_Error of HTTP.Status.Server_Error.t
  end
end
val body : agent:string -> uri:string
  -> out:(string:string -> offset:int -> length:int -> unit) -> string
