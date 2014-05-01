{
module Context
 = struct type t = string:string -> offset:int -> length:int -> unit end
module K
 = MfUe
module Lexing__
 = Lexing_.Async
 (struct
	type 'a t = 'a K.t
	let return = K.t
 end)
module Addr
 = struct
	module IPv4
	 = struct
		module Lexing
		 = IPv4_address.Async(Lexing__)
	 end
	module IPv6
	 = struct
		module Lexing
		 = IPv6_address.Async(Lexing__)
	 end
	module Host
	 = URI.Host
	module Port
	 = URI.Port
	module URI
	 = struct
		module Lexing
		 = URI.Async(Lexing__)(IPv4.Lexing)(IPv6.Lexing)
	 end
 end
module System
 = struct
	module Chan
	 = struct
		module Recv
		 = struct
			let into
			 = fun ~context ichn string ~ofs ~len ->
				let read = (Pervasives.input ichn string ofs len) in
				let out = context in
				out ~string ~offset:ofs ~length:read;
				Unix_error.t read
		 end
		module Send
		 = struct
			let string
			 = fun ~context ochn string ->
				(* Pervasives.prerr_string (">[" ^ string ^ "]"); *)
				Unix_error.t (Pervasives.output_string ochn string)
			let flush
			 = fun ~context ochn ->
				Unix_error.t (Pervasives.flush ochn)
		 end
	 end
	module Network
	 = struct
		module Addr
		 = struct
			module Inet
			 = struct
				let t
				 = fun ~context host ->
					match (host:Addr.Host.t) with
					| `IPv6 ipv6 -> K.t (IPv6_address.String.Inet_addr.t ipv6)
					| `IPv4 ipv4 -> K.t (IPv4_address.String.Inet_addr.t ipv4)
					| `IPvF _ -> assert false
					| `Name name ->
						let service = ""
						and info = [Unix.AI_FAMILY Unix.PF_INET]
						in
						let rec loop
						 = function
						 | { Unix
							 . ai_family=Unix.PF_INET
							 ; ai_addr=Unix.ADDR_INET (inet, _)
							 }::_ -> K.t inet
						 | _::list -> loop list
						in
						let name = Addr.Host.Name.String.t name ~encoded:false in
						loop (Unix.getaddrinfo name service info)
			 end
		 end
		module Conn
		 = struct
			module Data
			 = struct
				type t =
				 { ichn : Pervasives.in_channel
				 ; ochn : Pervasives.out_channel
				 }
				let t
				 = fun ~context ~port addr fct ->
					let (ichn, ochn) = Unix.open_connection
					 (Unix.ADDR_INET (addr, (port:URI.Port.t:>int))) in
					K.t' fct {ichn; ochn}
					 ~both:(fun _ -> Unix.shutdown_connection ichn)
					 ~ok: K.t
					 ~ko: Exception.ko
			 end
		 end
	 end
 end
module HTTP_
 = struct
	module Lexing
	 = HTTP.Async(Lexing__)(Addr.URI.Lexing)
	module Conn
	 = struct
		type t =
		 { host   : Addr.Host.t
		 ; port   : Addr.Port.t
		 ; send   : 'a. string -> (unit -> 'a Unix_error.t) -> 'a Unix_error.t
		 ; flush  : unit -> unit Unix_error.t
		 ; recv   : 'a. string -> len: int
			 -> (((unit -> 'a K.t) -> 'a K.t) -> int -> 'a K.t)
			 -> 'a K.t
		 ; lexbuf : Lexing__.t
		 }
		let t
		 : context: Context.t
		 -> host: Addr.Host.t
		 -> ?port: Addr.Port.t
		 -> (t -> 'data K.t)
		 -> 'data K.t
		 = fun ~context ~host ?(port=HTTP.Port.default) fct ->
			K.(&&)
			 (System.Network.Addr.Inet.t ~context)
			 (fun addr -> System.Network.Conn.Data.t ~context addr ~port
				 (fun {System.Network.Conn.Data.ichn; ochn} ->
					let send
					 = fun string cont ->
						Unix_error.(&&)
						 (System.Chan.Send.string ~context ochn)
						 cont
						 string
					and recv
					 = {Lexing__.fct=fun string ~len cont ->
						K.(&&)
						 (fun string -> K.Match_failure.t
							 (System.Chan.Recv.into ~context ichn ~ofs:0 ~len string))
						 (cont (fun cont -> cont ()))
						 string
					 }
					and flush
					 = fun () -> System.Chan.Send.flush ~context ochn
					in
					let lexbuf = Lexing__.from_function recv in
					fct
					 { host
					 ; port
					 ; send
					 ; recv=recv.Lexing__.fct
					 ; lexbuf
					 ; flush
					 }
				 )
			 ) host
	 end
	module Request
	 = struct
		module Agent
		 = URI.Pchar.Unreserved
		module System
		 = URI.Reg_name
		type t =
		 { agent  : Agent.t
		 ; system : System.t
		 ; path   : URI.Path.Absolute.t
		 }
		let send
		 = fun
		 { Conn
		 . host
		 ; port
		 ; send
		 ; flush
		 }
		 { agent
		 ; path
		 ; system
		 } ?query ~body ->
			HTTP.Emit.request
			 { HTTP.Request
			 . line=
				 { HTTP.Request.Line
				 . method_=`GET
				 ; uri=`Path (path, query)
				 ; version=HTTP.Version.v1_1
				 }
			 ; headers=
				[ `Host (host, Some port)
				; `Connection (Plus.Unsafe.t' [`Close])
				; `User_Agent
				 (Plus.Unsafe.t'
					 [ `Product
						 { HTTP.Product
						 . token=HTTP.Token.Unsafe.t' (agent:>string)
						 ; version=None
						 }
					 ; `Comment (List.map
						 (function
						 | `Unreserved unreserved -> `Ctext (HTTP.Ctext.Unsafe.t' (unreserved:URI.Pchar.Unreserved.t:>string))
						 | `Pct        char       -> `Quoted_pair char
						 | `Delim      char       -> `Quoted_pair (char:URI.Pchar.Delim.t:>char)
						 ) system)
					 ])
				]
			 ; body
			 } send flush
	 end
	module Response
	 = struct
		type t =
		 { body : Lexing__.lexbuf
		 }
		let recv
		 = fun
		 { Conn
		 . lexbuf
		 } ->
			Lexing.response
			 (fun
				 { HTTP.Response
				 . line=
					 { HTTP.Response.Line
					 . version
					 ; status
					 ; reason
					 }
				 ; headers
				 ; body=lexbuf
				 } ->
				Match_failure.t'
				 (function (`Success `OK:HTTP.Status.t) -> Match_failure.t {body=lexbuf})
				 status
				 ~ko: Exception.ko
				 ~ok: K.t
			 )
			 lexbuf
	 end
 end
module Async
 (Lexing:Lexing_.Async)
 = struct
}
	rule body
	 return
		= parse
		| (_* as string) { return string }
{
let rec body return lexbuf =
    __ocaml_lex_body_rec return lexbuf 0
and __ocaml_lex_body_rec return lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
  | 0 -> return (Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos)
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf (fun () -> __ocaml_lex_body_rec return lexbuf __ocaml_lex_state)

 end
module Sync
 = Async(Lexing__)

module Protocol
 = struct
	module Get
	 = struct
		module Body
		 = struct
			type t = string
			let t
			 = fun t -> t
			module Emit
			 = struct
				let t
				 = fun t send k ->
					send t k
			 end
		 end
		module Request
		 = struct
			type t =
			 { user : URI.User.t option
			 }
			let send
			 = fun
			 conn
			 ({ HTTP_.Request
			 . agent
			 } as http_request)
			 { user
			 } ->
				HTTP_.Request.send conn http_request
				 ?query: (match user with
					 | None -> None
					 | Some user -> Some (URI.Pchar.Unsafe.u "user"::URI.Pchar.Unsafe.d '='::(user:>URI.Query.t)))
		 end
		module Response
		 = struct
			module Result
			 = struct
				type t =
				 [ `OK
				 | `KO
				 ]
			 end
			type t =
			 { result : Result.t
			 ; body   : Body.t
			 }
			let default =
				{ result = `KO
				; body   = ""
				}
			let recv
			 = fun conn ->
				K.(&&)
				 HTTP_.Response.recv
				 (fun {HTTP_.Response.body=lexbuf} ->
					K.bind
					 (Sync.body K.t)
					 (fun body ->
						{ result = `OK
						; body
						})
					 lexbuf
				 ) conn
		 end
		let send
		 = fun ?(body="") http_request request conn ->
			Unix_error.(&&)
			 (Request.send conn http_request ~body:(Body.Emit.t body))
			 (fun () -> Response.recv conn)
			 request
	 end
 end

let body ~agent ~uri ~out =
  let agent = Match_failure.t'
    HTTP_.Request.Agent.String.t' agent
    ~ko:(fun _ -> assert false)
    ~ok: Exception.ok
  and system = Match_failure.t'
    HTTP_.Request.System.String.t' Sys.os_type
    ~ko:(fun _ -> assert false)
    ~ok: Exception.ok
  and user, host, port, path =
    match Match_failure.Unsafe.t (URI.String.t' uri) with
    |{ URI.scheme
     ; part=`Authority ({URI.Authority.host; port; user}, path)
     } when (scheme:URI.Scheme.t:>string) = "http" ->
      let path = Match_failure.Unsafe.t (URI.Path.Absolute.t path) in
      user, host, port, path
  in
  let i = Protocol.Get.send
    {HTTP_.Request.path; agent; system}
    {Protocol.Get.Request.user}
  in
  let mid = HTTP_.Conn.t ~context:out ~host ?port i in
  (K.Unsafe.t mid).Protocol.Get.Response.body
}
