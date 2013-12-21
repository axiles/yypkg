exception ChopList_ChopingTooMuch of (int * int)
exception ProcessFailed of (string * string option)
val cri : int
val err : int
val wrn : int
val dbg : int
val log : int -> ('a, out_channel, unit, unit) format4 -> 'a
val may : ('a -> unit) -> 'a option -> unit
val process_failed : ?stderr:string -> string array -> 'a
val os_type : [> `Unix | `Windows ]
val dir_sep : string
val run_and_read : string array -> [< `stderr | `stdout ] -> string
val split_by_line : string -> string list
val filename_concat : string list -> string
val binary_path : string
val install_path : string
val tar : string
val xz : string
val wget : string
val from_tar :
  [< `extract of string * string * string | `get of string | `list ] ->
  string -> string list
val split_path : ?dir_sep:string -> string -> string list
val chop_list : 'a list -> int -> 'a list
val strip_component :
  ?prefix:string -> ?dir_sep:string -> int -> string -> string
val read_file : string -> string Queue.t
val overwrite_file : string -> string Queue.t -> unit
val queue_map : ('a -> 'b) -> 'a Queue.t -> 'b Queue.t
val search_and_replace_in_file : string -> string -> string -> unit
val write_temp_file : string -> string -> string * string
val open_package : string -> Types.script
val rev_uniq : 'a list -> 'a list
val prepend_if : ('a -> bool) -> 'a list -> 'a -> 'a list
val rev_may_value : 'a option list -> 'a list
val assert_file_exists : string -> unit
exception Skip
val list_rev_map_skip : f:('a -> 'b) -> 'a list -> 'b list
val ep : ('a, out_channel, unit) format -> 'a
val sp : ('a, unit, string) format -> 'a
