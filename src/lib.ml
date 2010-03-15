open Printf
open Types

(* Install directory: current folder when the program is started... *)
let install_dir = Unix.getcwd ()

(* Simply make a version out of a string *)
let version_of_string s =
  (* we can factor this part, 'remaining' is handled later on *)
  let major, minor, release, remaining =
    Scanf.sscanf s "%d.%d.%d-%s" (fun a b c d -> a, b, c, d)
  in
  (* handle 'remaining' now *)
  let status, iter = match Str.split (Str.regexp "-") remaining with
    | [ "alpha"; x; y ] -> Alpha (int_of_string x), (int_of_string y)
    | [ "beta"; x ; y ] -> Beta (int_of_string x), (int_of_string y)
    | [ "rc"; x ; y ] -> RC (int_of_string x), (int_of_string y)
    | [ "snapshot"; x; y ] ->  Snapshot x, (int_of_string y)
    | [ "stable" ; y ] -> Stable, (int_of_string y)
    | _ -> assert false
  in
  {
    major = major;
    minor = minor;
    release = release;
    status = status;
    package_iteration = iter;
  }

(* create a string from a version *)
let string_of_version v =
  let status = 
    match v.status with
      | Alpha x -> sprintf "alpha-%d" x
      | Beta x -> sprintf "beta-%d" x
      | RC x -> sprintf "rc-%d" x
      | Snapshot s -> sprintf "snapshot-%s" s
      | Stable -> "stable"
  in
  sprintf "%d.%d.%d-%s-%d" v.major v.minor v.release status v.package_iteration

(* it would have been too dull if all OSes had the same directory separators *)
let dir_sep =
  match Sys.os_type with
    | "Unix"
    | "Cygwin" -> "/"
    | "Win32" -> "\\"
    | _ -> assert false

(* We expect tools in the installation directory *)

(* absolute paths to tar, xz, gzip and bzip2 *)
(* on windows, we use bsdtar and gnu tar on others *)
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

(* absolute path to the NamedPipe.exe executable, only makes sense on windows *)
let named_pipe () = 
  match Sys.os_type with
    | "Win32" -> Filename.concat install_dir "NamedPipe.exe"
    | "Unix" | "Cygwin" | _ -> assert false

(* guess the compressor (xz, gzip, bzip2) from the extension of a string *)
(* this function may raise a bunch of exceptions which should be caught with a
 * "try compressor_of_ext with _ -> ...": no need to be more specific, it only
 * means the user gave a wrong filename *)
let compressor_of_ext s =
  (* the extension is everything after the last dot in the string *)
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

(* tar + compress on unix, piping the output of tar to the compressor *)
let unix_tar_compress tar_args compress out =
  let s = String.concat " " ([ tar; "cv" ] @ (Array.to_list tar_args)) in
  let fst_out_channel = Unix.open_process_in s in
  let fst_out = Unix.descr_of_in_channel fst_out_channel in
  let second_out = Unix.openfile out [ Unix.O_WRONLY; Unix.O_CREAT ] 0o640 in
  let pid = Unix.create_process compress.(0) compress fst_out second_out Unix.stderr in
  ignore (Unix.waitpid [] pid);
  Unix.close fst_out;
  Unix.close second_out

(* tar + compress on windows, less trivial than its unix counterpart... *)
let win_tar_compress tar_args compress out =
  (* we have to rely on a named pipe (aka fifo) since pipes are unreliable *)
  let fifo_path = "\\\\.\\pipe\\makeypkg_compress" in
  let tar_args = Array.to_list tar_args in
  (* we tell tar to use the fifo as input *)
  let s = String.concat " " ([ tar; "cvf";  fifo_path ] @ tar_args) in
  (* NamedPipe.exe will redirect the named pipe to the compressor's input *)
  let named_pipe = [ [| named_pipe (); fifo_path |]; compress; [| "-c" |] ] in
  let named_pipe = Array.concat named_pipe in
  (* this is the output of the compressor *)
  let second_out = Unix.openfile out [ Unix.O_WRONLY; Unix.O_CREAT ] 0o640 in
  (* we start the compressor which waits from input on the named pipe *)
  let pid = Unix.create_process named_pipe.(0) named_pipe Unix.stdin second_out
  Unix.stderr in
  (* now we start tar which write to the named pipe *)
  let tar_oc = Unix.open_process_out s in
  ignore (Unix.close_process_out tar_oc);
  ignore (Unix.waitpid [] pid);
  Unix.close second_out

(* auto-dispatch between the unix and windows versions of *_tar_compress *)
let tar_compress tar_args compress out =
  match Sys.os_type with
    | "Cygwin"
    | "Unix" -> unix_tar_compress tar_args compress out
    | "Win32" -> win_tar_compress tar_args compress out
    | _ -> assert false

(* decompress + untar, "f" will read the output from tar:
 *   'tar xv -O' will output the content of files to stdout
 *   'tar xv' will output the list of files expanded to stdout
 *   'bsdtar xv -O' will output the content of files to stdout
 *   'bsdtar xv' will output the list of files expanded to stderr *)
let decompress_untar f tar_args input =
  let c = [| compressor_of_ext input; "-d"; "-c"; input |] in
  (* 'tar -f -' ensures we're reading from stdin with both gnu tar and bsd tar
   * bsdtar would default to /dev/tape0 otherwise *)
  let t = Array.append [| tar; "xvf"; "-" |] tar_args in
  let c_out, c_in = Unix.pipe () in
  let t_out, t_in = Unix.pipe () in
  let pid_c = Unix.create_process c.(0) c Unix.stdin c_in Unix.stderr in
  (* if we're using bsdtar and want the filelist, we have to read from stderr
   * see the comment right before the function for more details *)
  let pid_t = if "bsdtar.exe" = tar && List.mem "-O" (Array.to_list tar_args)
    then Unix.create_process t.(0) t c_out Unix.stdout t_in
    else Unix.create_process t.(0) t c_out t_in Unix.stderr
  in
  let t_out_chan = Unix.in_channel_of_descr t_out in
  (* bsdtar uses \r\n for end of lines, this will translate to \n only =) 
   * It has no effect on systems which already use \n *)
  set_binary_mode_in t_out_chan false;
  let s = f pid_t t_out_chan in
  (* f should return after the program pid_t has exited so no need to wait more
   * we're waiting for pid_c however even if it's probably not needed *)
  ignore (Unix.waitpid [] pid_c);
  ignore (List.iter Unix.close [ c_out; c_in; t_out; t_in ]);
  s

