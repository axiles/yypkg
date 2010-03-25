open Sexplib
open Types
open Yylib

let read path = 
  let ic = open_in path in
  let sexp = try Sexp.input_sexp ic with e -> close_in ic; raise e in
  close_in ic;
  sexp

let write path data =
  let flags = [ Open_creat; Open_binary; Open_wronly; Open_trunc ] in
  let oc = open_out_gen flags 0o644 path in
  let () = try Sexp.output_hum oc data with e -> close_out oc; raise e in
  close_out oc

