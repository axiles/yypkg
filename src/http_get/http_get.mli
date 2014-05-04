module HTTP_ : sig
  module Response : sig
    exception Client_Error of HTTP.Status.Client_Error.t
    exception Server_Error of HTTP.Status.Server_Error.t
  end
end
val body :
  agent:string
  -> user:URI.User.t option -> host:URI.Host.t -> port:URI.Port.t option
  -> path:URI.Path.Absolute.t
  -> out:(string:string -> offset:int -> length:int -> unit)
  -> string
