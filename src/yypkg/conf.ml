open Types
open Yylib

let set conf binding value =
  let conf = List.remove_assoc binding conf in
  (binding, value) :: conf

let unset conf binding = 
  List.remove_assoc binding conf

let read conf_path =
  conf_of_sexp (Disk.read conf_path)

let write conf_path conf =
  Disk.write conf_path (sexp_of_conf conf)
