open Printf
  
open Types
  
let version_of_string s =
  let (major, minor, release, remaining) =
    Scanf.sscanf s "%d.%d.%d-%s" (fun a b c d -> (a, b, c, d)) in
  let (status, iter) =
    match Str.split (Str.regexp "-") remaining with
    | [ "alpha"; x; y ] -> ((Alpha (int_of_string x)), (int_of_string y))
    | [ "beta"; x; y ] -> ((Beta (int_of_string x)), (int_of_string y))
    | [ "rc"; x; y ] -> ((RC (int_of_string x)), (int_of_string y))
    | [ "stable"; y ] -> (Stable, (int_of_string y))
    | _ -> assert false
  in
    {
      major = major;
      minor = minor;
      release = release;
      status = status;
      package_iteration = iter;
    }
  
let string_of_version v =
  let status =
    match v.status with
    | Alpha x -> sprintf "alpha-%d" x
    | Beta x -> sprintf "beta-%d" x
    | RC x -> sprintf "rc-%d" x
    | Stable -> "stable"
  in
    sprintf "%d.%d.%d-%s-%d" v.major v.minor v.release status
      v.package_iteration
  
let dir_sep =
  match Sys.os_type with
  | "Unix" | "Cygwin" -> "/"
  | "Win32" -> "\\"
  | _ -> assert false
  
let (tar, xz, gzip, bzip2) =
  match Sys.os_type with
  | "Unix" | "Cygwin" -> ("tar", "xz", "gzip", "bzip2")
  | "Win32" -> ("tar.exe", "xz.exe", "gzip.exe", "bzip2.exe")
  | _ -> assert false
  
let unix_tar_compress tar_args compress out =
  let s = String.concat " " ([ tar; "cv" ] @ (Array.to_list tar_args)) in
  let fst_out_channel = Unix.open_process_in s in
  let fst_out = Unix.descr_of_in_channel fst_out_channel in
  let second_out = Unix.openfile out [ Unix.O_WRONLY; Unix.O_CREAT ] 0o640 in
  let pid =
    Unix.create_process compress.(0) compress fst_out second_out Unix.stderr
  in
    (ignore (Unix.waitpid [] pid); Unix.close fst_out; Unix.close second_out)
  
let win_tar_compress tar_args compress out =
  let tar_args = Array.to_list tar_args in
  let fifo_path = "\\\\.\\pipe\\makeypkg_compress" in
  let s = String.concat " " ([ tar; "cvf"; fifo_path ] @ tar_args) in
  let named_pipe_a1 = [| "NamedPipe.exe"; fifo_path |] in
  let named_pipe_a2 = Array.append compress [| "-c" |] in
  let named_pipe = Array.append named_pipe_a1 named_pipe_a2 in
  let tar_oc = Unix.open_process_out s in
  let second_out = Unix.openfile out [ Unix.O_WRONLY; Unix.O_CREAT ] 0o640 in
  let pid =
    Unix.create_process named_pipe.(0) named_pipe Unix.stdin second_out Unix.
      stderr
  in
    (ignore (Unix.close_process_out tar_oc);
     ignore (Unix.waitpid [] pid);
     Unix.close second_out)
  
let tar_compress tar_args compress out =
  match Sys.os_type with
  | "Cygwin" | "Unix" -> unix_tar_compress tar_args compress out
  | "Win32" -> win_tar_compress tar_args compress out
  | _ -> assert false
  

