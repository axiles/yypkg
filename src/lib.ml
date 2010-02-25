open Printf
open Types

RE int = digit+
RE major_minor_release = 
  (int as maj : int) "." (int as min : int) "." (int as rel : int)

let version_of_string s =
  let /(major_minor_release "-" (_+ as s) "-" (int as iter : int))/ = s in
  let status = match s with
    | RE "alpha-" (int as x : int) -> Alpha x
    | RE "beta-" (int as x : int) -> Beta x
    | RE "rc-" (int as x : int) -> RC x
    | RE "stable" -> Stable
  in
  {
    major = maj;
    minor = min;
    release = rel;
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


