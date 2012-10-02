(** This module exists only in order to run the main by default only in
 * command-line mode (as opposed to GUI). *)

let () =
  Printexc.record_backtrace true;
  let b = Buffer.create 1000 in
  Yypkg_top.main_wrap_wrap b;
  Buffer.output_buffer stderr b;
  if Buffer.length b <> 0 then
    exit 1
