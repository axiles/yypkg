open Printf
open Types

let version_of_string s =
  let major, minor, release, remaining, iter =
    Scanf.sscanf s "%d.%d.%d-%s-%d" (fun a b c d e -> a, b, c, d, e)
  in
  let status = match Str.split (Str.regexp "-") remaining with
    | [ "alpha"; x ] -> Alpha (int_of_string x)
    | [ "beta"; x ] -> Beta (int_of_string x) 
    | [ "rc"; x ] -> RC (int_of_string x) 
    | [ "stable" ] -> Stable
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
  sprintf "%d.%d.%d-%s-%d" v.major v.minor v.release status v.package_iteration

let dir_sep =
  match Sys.os_type with
    | "Unix"
    | "Cygwin" -> "/"
    | "Win32" -> "\\"
    | _ -> assert false
