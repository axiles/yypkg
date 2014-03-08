exception ProcessFailed of (string * string option)
val cri : int
val err : int
val wrn : int
val dbg : int
val log : int -> ('a, out_channel, unit, unit) format4 -> 'a
val may : ('a -> unit) -> 'a option -> unit
val process_failed : ?stderr:string -> string array -> 'a
val os_type : [> `Unix | `Windows ]
val run_and_read : string array -> [< `stderr | `stdout ] -> string
val split_by_line : string -> string list
val filename_concat : string list -> string
val binary_path : string
val install_path : string
val tar : string
val xz : string
val wget : string
module Archive : sig
  module Transform : sig
    type t = string -> string option
    val strip_component : int -> t
    val filter : Str.regexp -> t
    val c : string -> t
    val wrap : t list -> ArchiveLow.Entry.t -> bool
  end
  val get_contents : archive:string -> file:string -> string
  val extract : ?transform:(ArchiveLow.Entry.t -> bool) -> string -> string list
  val list : string -> string list
end
val read_file : string -> string Queue.t
val overwrite_file : string -> string Queue.t -> unit
val search_and_replace_in_file : string -> string -> string -> unit
val open_package : string -> Types.script
val prepend_if : ('a -> bool) -> 'a list -> 'a -> 'a list
val rev_may_value : 'a option list -> 'a list
val assert_file_exists : string -> unit
exception Skip
val list_rev_map_skip : f:('a -> 'b) -> 'a list -> 'b list
val ep : ('a, out_channel, unit) format -> 'a
val sp : ('a, unit, string) format -> 'a
val string_count : string -> char -> int
