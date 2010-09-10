(*
 * yypkg - A cross-platforma package manager
 * Copyright (C) 2010 Adrien Nader
 * Copyright (C) <year>  <name of author>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Printf
open Types

exception ChopList_ChopingTooMuch of (int * int)
exception ProcessFailed

let list_rev_concat l = 
  List.fold_left (fun a b -> List.rev_append b a) [] l

let list_concat l =
  List.rev (list_rev_concat l)

let read pid descr =
  (* We'll be reading at most 160 characters at a time, I don't know if there's
   * a better way to do it: more, less, adptive. No idea but this should be good
   * enough *)
  let s = String.make 160 '_' in
  (* This function reads everything available from a descriptor and returns
   * when there's nothing more available (yet) *)
  let read_once descr =
    let rec read_once_rc accu =
      (* check if there's something to read: a timeout of 0.02 to minimize
       * latency, shouldn't cost anything *)
      match Unix.select [ descr ] [] [] 0.02 with
        | [ _ ],  _, _ -> begin
            match Unix.read descr s 0 160 with
              (* got as much as we asked for, there's probably more: try again*)
              | 160 as l -> read_once_rc ((String.sub s 0 l) :: accu)
              (* got less than asked, return *)
              | l -> (String.sub s 0 l) :: accu
          end
            (* ok, we got a timeout: return *)
        | _ -> accu
    in
    read_once_rc []
  in
  let rec read_rc pid descr accu =
    (* As long as the process is alive, we have to wait for it to send more data
     * even if nothing is available yet, but when it dies, it becomes unable to
     * write more and we can read everything available in one big pass *)
    match Unix.waitpid [ Unix.WNOHANG ] pid with
      (* still alive: read and start again, '0' because no child had its state
       * changed (we're using WNOHANG) *)
      | 0, _ -> read_rc pid descr ((read_once descr) :: accu)
      (* we know there won't be anything added now: we eat the remaining
       * characters and return right after that *)
      | pid, Unix.WEXITED 0 -> (read_once descr) :: accu
      (* all other cases, we'll say the process failed and raise an exception *)
      | _, _ -> raise ProcessFailed
  in
  let l = read_rc pid descr [] in
  let ll = List.fold_left (fun a b -> List.rev_append b a) [] l in
  let s = String.concat "" ll in
  (* Split on \r\n newlines on windows and \n newlines elsewhere *)
  match Sys.os_type with
    | "Win32" -> Str.split (Str.regexp "\r\n") s
    | _ (* Uniw | Cygwin *) -> Str.split (Str.regexp "\n") s

(* List.fold_left Filename.concat *)
let filename_concat = function
  | t :: q -> List.fold_left Filename.concat t q
  | [] -> raise (Invalid_argument "filename_concat, nothing to concat")

let binary_path =
  let dirname = Filename.dirname Sys.argv.(0) in
  if FilePath.DefaultPath.is_relative dirname then
    filename_concat [ Sys.getcwd (); dirname ]
  else
    dirname

let install_path =
  filename_concat [ binary_path; ".." ]

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

(* absolute paths to tar, xz, gzip and bzip2, and NamedPipe if on windows *)
(* on windows, we use bsdtar and gnu tar on others *)
let tar, xz, gzip, bzip2, named_pipe, wget = 
  match Sys.os_type with
    (* we don't set named_pipe for unix and cygwin because it's not used *)
    | "Unix"
    | "Cygwin" -> "bsdtar", "xz", "gzip", "bzip2", "", "wget"
    | "Win32" ->
        filename_concat [ binary_path; "bsdtar.exe" ],
        filename_concat [ binary_path; "xz.exe" ],
        filename_concat [ binary_path; "gzip.exe" ],
        filename_concat [ binary_path; "bzip2.exe" ],
        filename_concat [ binary_path; "NamedPipe.exe" ],
        filename_concat [ binary_path; "wget.exe" ]
    | _ -> assert false

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
  let second_out = Unix.openfile out [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644 in
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
  let named_pipe = [ [| named_pipe; fifo_path |]; compress; [| "-c" |] ] in
  let named_pipe = Array.concat named_pipe in
  (* this is the output of the compressor *)
  let second_out = Unix.openfile out [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644 in
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
 *   'bsdtar xv -O' will output the content of files to stdout
 *   'bsdtar xv' will output the list of files expanded to stderr *)
let decompress_untar ?(test=true) tar_args input =
  let compressor = compressor_of_ext input in
  let c = [| compressor; "-d"; "-c"; input |] in
  (* On windows, piping between the decompressor and tar is painfully slow,
   * hopefully bsdtar can link against lib{lzma,z,bz2}, avoiding the need to
   * pipe between process which makes it 100 times faster (number not made
   * up...) so we'll try to take advantage of that *)
  match Sys.os_type, Sys.file_exists compressor with
    | "Win32", false -> 
        let t = Array.append [| tar; "xvf"; input |] tar_args in
        let t_out, t_in = Unix.pipe () in
        (* same as in the other branch : we are always using bsdtar here and if
          * we want the filelist, we need to read stderr *)
        let pid_t = if List.mem "-O" (Array.to_list tar_args) then
          Unix.create_process t.(0) t Unix.stdin t_in Unix.stderr
        else
          Unix.create_process t.(0) t Unix.stdin Unix.stdout t_in
        in
        read pid_t t_out
    | _ -> 
        (* bsdtar defaults to /dev/tape0 with "-f '-'" *)
        let t = Array.append [| tar; "xvf"; "-" |] tar_args in
        let c_out, c_in = Unix.pipe () in
        let t_out, t_in = Unix.pipe () in
        let pid_c = Unix.create_process c.(0) c Unix.stdin c_in Unix.stderr in
        (* if we're using bsdtar and want the filelist, we have to read from
         * stderr: see the comment right before the function for more details *)
        let pid_t =
          if List.mem "-O" (Array.to_list tar_args) then
            Unix.create_process t.(0) t c_out t_in Unix.stderr
          else
            Unix.create_process t.(0) t c_out Unix.stdout t_in
        in
        let l = read pid_t t_out in
        (* let's clean the compressor from the process table *)
        ignore (Unix.waitpid [ Unix.WNOHANG ] pid_c);
        List.iter Unix.close [ c_out; c_in; t_out; t_in ];
        l

let split_path ?(dir_sep=dir_sep) path =
  Str.split (Str.regexp dir_sep) path

(* chop_list list i removes the first i elements of list and raises
 * ChopList_ChopingTooMuch if the list is shorter than i *)
let chop_list list i =
  let rec chop_list_rc j = function
    | l when j = 0 -> l
    | t :: q -> chop_list_rc  (j-1) q
    | [] ->
        raise (ChopList_ChopingTooMuch (List.length list, i))
    (* this means we're trying to chop more than possible, 'l when i = 0'
     * handles the case when we're trying to chop as much as we have so we
     * can simply always yell here *)
  in
  chop_list_rc i list

(* Remove the first 'n' components of a path (string list) and optionaly
 * prepends a prefix
 * That sounds a bit weird because I started changing how yypkg handled this but
 * never finished *)
let strip_component ?prefix ?dir_sep n path =
  match prefix with
    | None -> filename_concat (chop_list (split_path ?dir_sep path) n)
    | Some prefix -> filename_concat (prefix :: (chop_list (split_path ?dir_sep path) n))

(* read a file line-by-line and return its contents in a string Queue.t *)
let read_file file =
  let in_channel = open_in_bin file in
  let q : string Queue.t = Queue.create () in
  (try
    while true do
      Queue.push (input_line in_channel) q
    done
  with End_of_file -> ());
  close_in in_channel;
  q

let overwrite_file file contents =
  (* TODO: use the right permissions (same as original file) *)
  let out_channel = open_out_gen [ Open_binary; Open_trunc; Open_wronly ] 0o644
  file in
  let output_end_line oc s = output_string oc s; output_char oc '\n' in
  Queue.iter (output_end_line out_channel) contents;
  close_out out_channel

(* The sadly non-existant Queue.map *)
let queue_map f q =
  let new_queue = Queue.create () in
  Queue.iter (fun x -> Queue.push (f x) new_queue) q;
  new_queue

(* Search for a regexp in a file's lines and Str.global_replace *)
let search_and_replace_in_file file search replace =
  let contents = read_file file in
  let f = Str.global_replace (Str.regexp search) replace in
  let new_contents = queue_map f contents in
  overwrite_file file new_contents

let write_temp_file base_name contents =
  let dir = Filename.temp_dir_name in
  let path = Filename.concat dir base_name in
  let oc = open_out_bin path in
  let () = output_string oc contents in
  let () = close_out oc in
  dir, base_name

(* reads 'package_script.el' from a package *)
let open_package package =
  let l = decompress_untar [| "-O"; "package_script.el" |] package in
  let s = String.concat "\n" l in
  script_of_sexp (Sexplib.Sexp.of_string s)

