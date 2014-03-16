(*
 * yypkg - A cross-platform package manager
 * Copyright (C) 2010-2014 Adrien Nader
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

open Types

exception ProcessFailed of (string * string option)
exception Skip

let ep = Printf.eprintf
let sp = Printf.sprintf

let cri = 0
let err = 1
let wrn = 2
let dbg = 3

let log level =
  let threshold =
    try
      match Sys.getenv "YYLOGLEVEL" with
      | "CRI" | "cri" -> cri
      | "ERR" | "err" -> err
      | "WRN" | "wrn" -> wrn
      | "DBG" | "dbg" -> dbg
      | s -> try int_of_string s with _ -> 0
    with Not_found -> 0
  in
  (if threshold >= level then Printf.kfprintf else Printf.ikfprintf)
  (fun _ -> ()) stderr

let may f = function
  | None -> ()
  | Some v -> f v

let process_failed ?stderr a =
  let s = String.concat " " (Array.to_list a) in
  ep "Command `%s' failed.\n%!" s;
  may (ep "Here is the content of stderr:\n%s%!") stderr;
  raise (ProcessFailed (s, stderr))

let os_type =
  match Sys.os_type with
  | "Unix" | "Cygwin" -> `Unix
  | "Win32" -> `Windows
  | _ -> assert false

let read pid ~accumulate ~output =
  (* We'll be reading at most 160 characters at a time, I don't know if there's
   * a better way to do it: more, less, adaptive. No idea but this should be
   * good enough *)
  let s = String.make 160 '_' in
  (* This function reads everything available from a descriptor and returns
   * when there's nothing more available (yet) *)
  let rec read_once ((fd_out, buf_out) as out) ((fd_err, buf_err) as err) =
    (* check if there's something to read: a timeout of 0.02 to minimize
     * latency, shouldn't cost anything *)
    let lst_read, _, _ = Unix.select [ fd_out; fd_err ] [] [] 0.02 in
    let f fd buf =
      let l = Unix.read fd s 0 160 in
      Buffer.add_substring buf s 0 l;
      l
    in
    let l_out = if List.mem fd_out lst_read then f fd_out buf_out else 0 in
    let l_err = if List.mem fd_err lst_read then f fd_err buf_err else 0 in
    if l_out = 160 || l_err = 160 then read_once out err else ()
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
  f (stdout, accu.stdout) (stderr, accu.stderr)

let run_and_read argv which_fd =
  let stdout_out, stdout_in = Unix.pipe () in
  let stderr_out, stderr_in = Unix.pipe () in
  let output = { stdout = Buffer.create 20000; stderr = Buffer.create 20000 } in
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
  let line_end = match os_type with
  | `Unix -> "\n"
  | `Windows -> "\r\n"
  in
  Str.split (Str.regexp line_end) s

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

(* absolute paths to bsdtar, xz and wget *)
let tar, xz =
  match os_type with
  | `Unix -> "bsdtar", "xz"
  | `Windows ->
      filename_concat [ binary_path; "bsdtar.exe" ],
      filename_concat [ binary_path; "xz.exe" ]

module Archive = struct
  module A = ArchiveLow

  module Transform = struct
    type t = string -> string option
    let strip_component n =
      fun path ->
        try
          let i = String.index path '/' in
          if i = String.length path - 1 then
            None
          else
            Some (String.sub path (i+1) (String.length path - (i+1)))
        with Not_found ->
          None

    let filter re =
      fun s ->
        if Str.string_match re s 0 then
          Some s
        else
          None

    let c prefix =
      fun s ->
        Some (String.concat "/" [ prefix; s ])

    let wrap fl =
      let may_map_r v f =
        match v with
        | None -> None
        | Some v -> f v
      in
      fun e ->
        let p = List.fold_left may_map_r (Some (A.Entry.pathname e)) fl in
        may (A.Entry.set_pathname e) p;
        p <> None
  end

  type archive =
    | Filename of string
    | String of string
    | Bigarray of (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let with_archive_and_entry ~archive ~f =
    let t = A.Read.create () in
    let e = A.Entry.create () in
    A.Read.support_filter_all t;
    A.Read.support_format_all t;
    (match archive with
    | Filename file -> A.Read.open_filename t file (16 * 1024)
    | String ba -> A.Read.open_string t ba
    | Bigarray ba -> A.Read.open_bigarray t ba);
    try
      let res = f t e in
      A.Read.close t;
      res
    with exn ->
      A.Read.close t;
      raise exn

  let rec get_contents file t e =
    if A.Read.next_header2 t e then
      if A.Entry.pathname e = file then
        let l = Int64.to_int (A.Entry.stat e).Unix.LargeFile.st_size in
        let b = String.create l in
        let _len = A.Read.data t b 0 l in
        b
      else
        get_contents file t e
    else
      raise Not_found

  let get_contents ~archive ~file =
    with_archive_and_entry ~f:(get_contents file) ~archive

  let extract ?(transform=(fun _ -> true)) t e =
    let flags = A.Read.([ OWNER; PERM; TIME ]) in
    let rec aux accu =
      if A.Read.next_header2 t e then
        if transform e then
          let () = A.Read.extract t e flags in
          aux (A.Entry.pathname e :: accu)
        else
          aux accu
      else
        List.rev accu
    in
    aux []

  let extract ?transform archive =
    with_archive_and_entry ~f:(extract ?transform) ~archive

  let rec list accu t e =
    if A.Read.next_header2 t e then
      list (A.Entry.pathname e :: accu) t e
    else
      List.rev accu

  let list archive =
    with_archive_and_entry ~f:(list []) ~archive
end

(* read a file line-by-line and return its contents in a string Queue.t *)
let read_file_q file =
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
  let oc = open_out_gen [ Open_binary; Open_trunc; Open_wronly ] 0o644 file in
  Queue.iter (fun s -> output_string oc s; output_char oc '\n') contents;
  close_out oc

(* The sadly non-existant Queue.map *)
let queue_map f q =
  let new_queue = Queue.create () in
  Queue.iter (fun x -> Queue.push (f x) new_queue) q;
  new_queue

(* Search for a regexp in a file's lines and Str.global_replace *)
(* FIXME: is this function really needed and used? *)
let search_and_replace_in_file file search replace =
  let search = Str.regexp search in
  let contents = read_file_q file in
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

(* reads 'package_script.el' from a package *)
let open_package package =
  let archive = Archive.Filename package in
  let s = Archive.get_contents ~archive ~file:"package_script.el" in
  TypesSexp.To.script (Pre_sexp.of_string s)

let prepend_if pred accu x =
  if pred x then x :: accu else accu

(* check a file exists: raises an exception with the name of the missing file if
  * it doesn't *)
let assert_file_exists f =
  if not (Sys.file_exists f) then
    raise (File_not_found f)

let list_rev_map_skip ~f l =
  let rec aux f accu = function
    | hd :: tl ->
        let accu = try (f hd) :: accu with Skip -> accu in
        aux f accu tl
    | [] -> accu
  in
  aux f [] l

let rev_may_value l =
  list_rev_map_skip (function Some x -> x | None -> raise Skip) l

let string_count s c =
  let n = ref 0 in
  String.iter (fun c2 -> if c2 = c then incr n) s;
  !n

let sha3_file file =
  let ic = open_in_bin file in
  let sha3 = Cryptokit.hash_channel (Cryptokit.Hash.sha3 512) ic in
  close_in ic;
  sha3
