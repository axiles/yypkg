val cli_spec : Args.child
val mirror : unit -> string option
val init : prefix:string -> host_triplet:string -> host_system:string -> mirror:string -> bits:int -> unit
val main : Args.opt list -> unit
