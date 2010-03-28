open Printf

TYPE_CONV_PATH "Types"

(* this has to be kept ordered !!! *)
type status = 
  | Alpha of int
  | Beta of int
  | RC of int
  | Snapshot of string
  | Stable

type version = {
  major : int;
  minor : int;
  release : int;
  status : status;
  package_iteration : int;
}

(* both don't behave the same way of course *)
type tar_kind = BSD | GNU

type absolute_path = string with sexp
type relative_path = string with sexp

(* paths outside the package, on the hard drive *)
type outside_path = string with sexp

(* paths inside the package *)
type inside_path = relative_path with sexp

type param = string with sexp
type params = string list with sexp
type argv = string list with sexp

type action_id = string with sexp

type install_action =
  | AHK of params
  | Exec of argv
  | Expand of inside_path * outside_path
  | MKdir of outside_path
with sexp

type uninstall_action =
  | RM of outside_path
  | Reverse of action_id
with sexp

type results = 
  | Filelist of string list
  | NA
with sexp

type metadata = {
  package_name : string;
  package_size_expanded : string;
  package_version : string;
  packager_email : string;
  packager_name : string;
  description : string;
} with sexp

type script =
  metadata
  * (action_id * install_action) list
  * uninstall_action list
with sexp

type package = script * (action_id * results) list with sexp

type db = package list with sexp

type conf = (string * string) list with sexp

