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
module U = Unix

exception ChopList_ChopingTooMuch of (int * int)
exception ProcessFailed of (string * string option)

let process_failed ?stderr a =
  let s = String.concat " " (Array.to_list a) in
  Printf.eprintf "Command `%s' failed.\n%!" s;
  (match stderr with
  | Some stderr -> Printf.eprintf "Here is the content of stderr:\n%s%!" stderr
  | None -> ());
  raise (ProcessFailed (s, stderr))

let os_type =
  match Sys.os_type with
  | "Unix" | "Cygwin" -> `Unix
  | "Win32" -> `Windows
  | _ -> assert false

(* it would have been too dull if all OSes had the same directory separators *)
let dir_sep =
  match os_type with
  | `Unix -> "/"
  | `Windows -> "\\"

let read pid ~accumulate ~output =
  (* We'll be reading at most 160 characters at a time, I don't know if there's
   * a better way to do it: more, less, adptive. No idea but this should be good
   * enough *)
  let s = String.make 160 '_' in
  (* This function reads everything available from a descriptor and returns
   * when there's nothing more available (yet) *)
  let rec read_once descr buf =
    (* check if there's something to read: a timeout of 0.02 to minimize
     * latency, shouldn't cost anything *)
    match Unix.select [ descr ] [] [] 0.02 with
      | [ _ ],  _, _ -> begin
          let l = Unix.read descr s 0 160 in
          Buffer.add_substring buf s 0 l;
          if l = 160 then read_once descr buf else ()
        end
          (* ok, we got a timeout: return *)
      | _ -> ()
  in
  let rec read_rc pid accumulate accu =
    (* As long as the process is alive, we have to wait for it to send more data
     * even if nothing is available yet, but when it dies, it becomes unable to
     * write more and we can read everything available in one big pass *)
    match Unix.waitpid [ Unix.WNOHANG ] pid with
      (* still alive: read and start again, '0' because no child had its state
       * changed (we're using WNOHANG) *)
      | 0, _ -> accumulate read_once accu; read_rc pid accumulate accu
      (* we know there won't be anything added now: we eat the remaining
       * characters and return right after that *)
      | pid, status -> accumulate read_once accu; status
  in
  read_rc pid accumulate output

type process_output = {
  stdout : Buffer.t;
  stderr : Buffer.t;
}

let accumulate ~stdout ~stderr f accu =
  f stdout accu.stdout;
  f stderr accu.stderr

let run_and_read argv which_fd =
  let stdout_out, stdout_in = Unix.pipe () in
  let stderr_out, stderr_in = Unix.pipe () in
  let output = { stdout = Buffer.create 10000; stderr = Buffer.create 10000 } in
  let accumulate = accumulate ~stdout:stdout_out ~stderr:stderr_out in
  let pid = Unix.create_process argv.(0) argv Unix.stdin stdout_in stderr_in in
  let status = read pid ~accumulate ~output in
  List.iter Unix.close [ stdout_out; stdout_in; stderr_out; stderr_in ];
  match status with
  | Unix.WEXITED 0 ->
      (match which_fd with
      | `stdout -> Buffer.contents output.stdout
      | `stderr -> Buffer.contents output.stderr)
  | _ -> process_failed ~stderr:(Buffer.contents output.stderr) argv

let split_by_line s =
  Str.split (Str.regexp "\n") s

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

(* We expect tools in the installation directory *)

(* absolute paths to bsdtar, xz and wget *)
let tar, xz, wget = 
  match os_type with
  | `Unix -> "bsdtar", "xz", "wget"
  | `Windows ->
      filename_concat [ binary_path; "bsdtar.exe" ],
      filename_concat [ binary_path; "xz.exe" ],
      filename_concat [ binary_path; "wget.exe" ]

let config_guess1, config_guess2 =
  match os_type with
  | `Unix ->
      "/usr/share/libtool/config/config.guess",
      "/usr/share/libtool/config.guess"
  | `Windows -> "", ""

(* tar + compress on unix, piping the output of tar to the compressor *)
let tar_compress tar_args compress out =
  let tar_args = Array.concat [ [| tar; "cvf"; "-" |]; tar_args ] in
  let fst_out, fst_in = U.pipe () in
  let snd_out = U.openfile out [ U.O_WRONLY; U.O_CREAT; U.O_TRUNC ] 0o644 in
  let pid1 = U.create_process tar tar_args U.stdin fst_in U.stderr in
  U.close fst_in;
  let pid2 = U.create_process compress.(0) compress fst_out snd_out U.stderr in
  U.close fst_out; U.close snd_out;
  match snd (U.waitpid [] pid2), snd (U.waitpid [] pid1) with
  | U.WEXITED 0, U.WEXITED 0 -> ()
  | _, _ -> process_failed tar_args 

(* decompress + untar, "f" will read the output from bsdtar:
 *   'bsdtar xv -O' outputs the content of files to stdout
 *   'bsdtar xv' outputs the list of files expanded to stderr
 *   'bsdtar t' outputs the list of files to stdout *)
let from_tar action input =
  let tar_argv, which_fd = match action with
  | `extract (pq, strip, iq) ->
      [| tar; "xvf"; input; "--strip-components"; strip |], `stderr
  | `get file -> [| tar; "xf"; input; "-qO"; file |], `stdout
  | `list -> [| tar; "tf"; input |], `stdout
  in
  split_by_line (run_and_read tar_argv which_fd)

let split_path ?(dir_sep=dir_sep) path =
  Str.split_delim (Str.regexp dir_sep) path

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
  let common = chop_list (split_path ?dir_sep path) n in
  match prefix with
  | None -> filename_concat common
  | Some prefix -> filename_concat (prefix :: common)

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
  let search = Str.regexp search in
  let contents = read_file file in
  (* It's possible that one replace makes a previously-impossible replace
   * possible. An example is simplifying "foo/bar/baz/../..". If we simply do
   * 's;[^/]\+/\+\.\.;/;', we'll be left with "foo/bar/.." 
   * In other words: we repeat until we reach a fixpoint *)
  let rec f s =
    let s1 = Str.global_replace search replace s in
    if s = s1 then s else f s1
  in
  let new_contents = queue_map f contents in
  overwrite_file file new_contents

let write_temp_file base_name contents =
  let dir = Filename.temp_dir_name in
  let path = Filename.concat dir base_name in
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc;
  dir, base_name

(* reads 'package_script.el' from a package *)
let open_package package =
  let l = from_tar (`get "package_script.el") package in
  let s = String.concat "\n" l in
  TypesSexp.To.script (Sexplib.Sexp.of_string s)

let rev_uniq l =
  let rec rev_uniq_rc accu cur = function
    | t :: q when t = cur -> rev_uniq_rc accu cur q
    | t :: q -> rev_uniq_rc (t :: accu) t q
    | [] -> accu
  in
  match l with
  | t :: q -> rev_uniq_rc [ t ] t q
  | [] -> []

let list_rev_map_exn f l =
  let rec aux f accu = function
    | t :: q ->
        let x = try Some (f t) with _ -> None in
        (match x with
        | Some x -> aux f (x :: accu) q
        | None -> aux f accu q)
    | [] -> accu
  in
  aux f [] l

let prepend_if pred accu x =
  if pred x then x :: accu else accu

let rev_may_value l =
  let f = function
    | Some x -> x
    | None -> assert false
  in
  List.rev_map f (List.filter ((<>) None) l)

let guess_arch () = (* XXX: hmmmm; config.guess and a hardcoded triplet... *)
  match os_type with
  | `Unix -> 
      let host =
        try run_and_read [| config_guess1 |] `stdout with
        | ProcessFailed _ -> run_and_read [| config_guess2 |] `stdout
      in
      (* This gets the first line of input and remove any trailing newline:
        * # Scanf.sscanf "truc\n" "%s" (fun s -> s);;  
        * - : string = "truc"
        * # Scanf.sscanf "truc\naaaaaaaa" "%s" (fun s -> s);;
        * - : string = "truc" *)
      Scanf.sscanf host "%s" (fun s -> s)
  | `Windows -> "i686-w64-mingw32"

(* check a file exists: raises an exception with the name of the missing file if
  * it doesn't *)
let assert_file_exists f =
  if not (Sys.file_exists f) then
    raise (File_not_found f)

