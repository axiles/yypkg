exception ProcessFailed of (string * string option)
exception Skip
val cwd : string
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
val make_absolute_if_not : ?cwd:string -> string -> string
val absolute_executable_name : unit -> string
val install_path : string option
module Archive : sig
  module Transform : sig
    type t = string -> string option
    val strip_prefix_length : int -> t
    val filter : Str.regexp -> t
    val c : string -> t
    val wrap : t list -> ArchiveLow.Entry.t -> bool
  end
  type archive =
    | Filename of string
    | String of string
    | Bigarray of (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  val get_contents : archive:archive -> file:string -> string
  val extract : ?transform:(ArchiveLow.Entry.t -> bool) -> archive -> string list
  val list : archive -> string list
end
val overwrite_file : string -> string Queue.t -> unit
val search_and_replace_in_file : string -> string -> string -> unit
val open_package : string -> Types.script
val rev_uniq : 'a list -> 'a list
val prepend_if : ('a -> bool) -> 'a list -> 'a -> 'a list
val rev_may_value : 'a option list -> 'a list
val assert_file_exists : string -> unit
val list_rev_map_skip : f:('a -> 'b) -> 'a list -> 'b list
val ep : ('a, out_channel, unit) format -> 'a
val sp : ('a, unit, string) format -> 'a
val string_count : string -> char -> int
val sha3_file : string -> string
val started_from_windows_gui : unit -> bool
val int64_of_fileutil_size : FileUtil.size -> int64
