open Printf
open Types

let install_dir = Unix.getcwd ()

let version_of_string s =
  let major, minor, release, remaining =
    Scanf.sscanf s "%d.%d.%d-%s" (fun a b c d -> a, b, c, d)
  in
  let status, iter = match Str.split (Str.regexp "-") remaining with
    | [ "alpha"; x; y ] -> Alpha (int_of_string x), (int_of_string y)
    | [ "beta"; x ; y] -> Beta (int_of_string x), (int_of_string y)
    | [ "rc"; x ; y] -> RC (int_of_string x), (int_of_string y)
    | [ "stable" ; y] -> Stable, (int_of_string y)
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

let tar, xz, gzip, bzip2 =
  match Sys.os_type with
    | "Unix"
    | "Cygwin" -> "tar", "xz", "gzip", "bzip2"
    | "Win32" ->
        let bsdtar = Filename.concat install_dir "bsdtar.exe" in
        let xz = Filename.concat install_dir "xz.exe" in
        let gzip = Filename.concat install_dir "gzip.exe" in
        let bzip2 = Filename.concat install_dir "bzip2.exe" in
        bsdtar, xz, gzip, bzip2
    | _ -> assert false

let named_pipe () = 
  match Sys.os_type with
    | "Win32" -> Filename.concat install_dir "NamedPipe.exe"
    | "Unix" | "Cygwin" | _ -> assert false

let compressor_of_ext s =
  (* this function may raise a bunch of exceptions which should be caught with a
   * "try compressor_of_ext with _ -> ...": no need to be more specific, it only
   * means the user gave a wrong filename *)
  let ext_of_filename s =
    let l = String.length s in
    let i = String.rindex s '.' in
    String.sub s (i+1) (l-i-1)
  in
  match ext_of_filename s with
    | "tgz" -> gzip
    | "txz" -> xz
    | "tbz2" -> bzip2
    | _ -> assert false

let unix_tar_compress tar_args compress out =
  let s = String.concat " " ([ tar; "cv" ] @ (Array.to_list tar_args)) in
  let fst_out_channel = Unix.open_process_in s in
  let fst_out = Unix.descr_of_in_channel fst_out_channel in
  let second_out = Unix.openfile out [ Unix.O_WRONLY; Unix.O_CREAT ] 0o640 in
  let pid = Unix.create_process compress.(0) compress fst_out second_out Unix.stderr in
  ignore (Unix.waitpid [] pid);
  Unix.close fst_out;
  Unix.close second_out

let win_tar_compress tar_args compress out =
  let tar_args = Array.to_list tar_args in
  let fifo_path = "\\\\.\\pipe\\makeypkg_compress" in
  let s = String.concat " " ([ tar; "cvf";  fifo_path ] @ tar_args) in
  let named_pipe_a1 = [| named_pipe (); fifo_path |] in
  let named_pipe_a2 = Array.append compress [| " -c" |] in
  let named_pipe = Array.append named_pipe_a1 named_pipe_a2 in
  print_endline s;
  print_endline (String.concat " " (Array.to_list named_pipe));
  let second_out = Unix.openfile out [ Unix.O_WRONLY; Unix.O_CREAT ] 0o640 in
  let pid = Unix.create_process named_pipe.(0) named_pipe Unix.stdin second_out
  Unix.stderr in
  let tar_oc = Unix.open_process_out s in
  ignore (Unix.close_process_out tar_oc);
  ignore (Unix.waitpid [] pid);
  Unix.close second_out

let tar_compress tar_args compress out =
  match Sys.os_type with
    | "Cygwin"
    | "Unix" -> unix_tar_compress tar_args compress out
    | "Win32" -> win_tar_compress tar_args compress out
    | _ -> assert false

let unix_decompress_untar f tar_args input =
  let compressor = compressor_of_ext input in
  let s = String.concat " " [ compressor; "-d"; "-c"; input ] in
  let tar = Array.append [| tar; "xvf"; "-" |] tar_args in
  let fst_out_channel = Unix.open_process_in s in
  let fst_out = Unix.descr_of_in_channel fst_out_channel in
  let second_out, second_in = Unix.pipe () in
  let pid = Unix.create_process tar.(0) tar fst_out second_in Unix.stderr in
  let sexp = f (Unix.in_channel_of_descr second_out) in
  ignore (Unix.waitpid [] pid);
  ignore (Unix.close second_out, Unix.close second_in);
  Unix.close fst_out;
  sexp

let win_decompress_untar f tar_args input =
  let fifo_path = "\\\\.\\pipe\\makeypkg_decompress" in
  let compressor = [| compressor_of_ext input; "-d"; "-c"; input |] in
  let tar_args = Array.to_list tar_args in
  let named_pipe = [ named_pipe (); fifo_path; tar; " xvf"; "-" ] @ tar_args in
  let named_pipe = String.concat " " named_pipe in
  print_endline (String.concat " " (Array.to_list compressor));
  print_endline named_pipe;
  let (second_out, _, second_err) as second =
    Unix.open_process_full named_pipe (Unix.environment ()) in
  set_binary_mode second_err false;
  Unix.sleep 1;
  let compressor_out = Unix.openfile fifo_path [ Unix.O_WRONLY ] 0o640 in
  let pid = Unix.create_process compressor.(0) compressor Unix.stdin
  compressor_out Unix.stderr in
  let sexp = if List.mem "-O" tar_args then f second_out else f second_err in
  ignore (Unix.close_process_full second);
  ignore (Unix.waitpid [] pid);
  Unix.close compressor_out;
  sexp

let decompress_untar f tar_args input =
  match Sys.os_type with
    | "Cygwin"
    | "Unix" -> unix_decompress_untar f tar_args input
    | "Win32" -> win_decompress_untar f tar_args input
    | _ -> assert false

