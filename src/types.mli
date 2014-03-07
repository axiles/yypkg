type source_version = string
type version = string * int
val string_of_version : string * int -> string
val dummy_version : unit -> string * int
type action_id = string
type filekind = [ `Directory | `File | `Unhandled of string ]
type install_action =
    AHK of string list
  | Exec of string list
  | Expand of string * string
  | MKdir of string
  | SearchReplace of string * string * string
  | Symlink of string * string * filekind
type result = string list
type uninstall_action = RM of string | Reverse of action_id
type predicate = string * string list
exception Unmatched_predicates of (string * string) list
type size = FileUtil.size
type metadata = {
  name : string;
  size_expanded : size;
  version : version;
  packager_email : string;
  packager_name : string;
  description : string;
  host : string;
  target : string option;
  predicates : (string * string) list;
  comments : string list;
}
type script =
    metadata * (action_id * install_action) list * uninstall_action list
type package = script * (action_id * result) list
type db = package list
type conf = {
  predicates : predicate list;
  mirror : string;
}
type pkg = {
  metadata : metadata;
  size_compressed : size;
  filename : string;
  signature : string option;
  files : string list;
  deps : string list;
  sha3 : string;
}
type repository = { target : string; host : string; pkglist : pkg list; }

exception Package_does_not_exist
exception File_not_found of string
exception Not_upgrading_not_installed_package of string
