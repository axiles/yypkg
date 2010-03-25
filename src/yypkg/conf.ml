open Types
open Yylib

let set conf binding value =
  let conf = List.remove_assoc binding conf in
  (binding, value) :: conf

let unset conf binding = 
  List.remove_assoc binding conf

let read () =
  conf_of_sexp (Disk.read conf_path)

let write conf =
  (* Let's sort the configuration. Won't be faster but should be nicer to read
   * when editing the file by hand. It'll also avoid requiring to sort the
   * output when listing the configuration on command-line.
   * We use stable_sort so not to change anything if there are several bindings
   * for the same value, it shouldn't happen but Murphy's Law is Murphy's Law,
   * so why not stay safe? *)
  let sorted_conf = List.stable_sort compare conf in
  Disk.write conf_path (sexp_of_conf sorted_conf)
