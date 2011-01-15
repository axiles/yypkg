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
      match U.select [ descr ] [] [] 0.02 with
        | [ _ ],  _, _ -> begin
            match U.read descr s 0 160 with
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
    match U.waitpid [ U.WNOHANG ] pid with
      (* still alive: read and start again, '0' because no child had its state
       * changed (we're using WNOHANG) *)
      | 0, _ -> read_rc pid descr ((read_once descr) :: accu)
      (* we know there won't be anything added now: we eat the remaining
       * characters and return right after that *)
      | pid, U.WEXITED 0 -> (read_once descr) :: accu
      (* all other cases, we'll say the process failed and raise an exception *)
      | _, _ -> raise ProcessFailed
  in
  let l = read_rc pid descr [] in
  let ll = List.fold_left (fun a b -> List.rev_append b a) [] l in
  Str.split (Str.regexp "\n") (String.concat "" ll)

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

let dummy_meta () =
  let version = dummy_version () in
  let size_expanded = FileUtil.TB (Int64.of_int 42) in
  let meta = { name = "dummy_name"; size_expanded = size_expanded; version =
    version; packager_email = "nobody@example.com"; packager_name = "ulysse";
    description = "dummy"; predicates = []; comments = [] }
  in
  Sexplib.Sexp.to_string_hum (sexp_of_metadata meta)

(* it would have been too dull if all OSes had the same directory separators *)
let dir_sep =
  match Sys.os_type with
    | "Unix"
    | "Cygwin" -> "/"
    | "Win32" -> "\\"
    | _ -> assert false

(* We expect tools in the installation directory *)

(* absolute paths to bsdtar, xz and wget *)
let tar, xz, wget = 
  match Sys.os_type with
    | "Unix"
    | "Cygwin" -> "bsdtar", "xz", "wget"
    | "Win32" ->
        filename_concat [ binary_path; "bsdtar.exe" ],
        filename_concat [ binary_path; "xz.exe" ],
        filename_concat [ binary_path; "wget.exe" ]
    | _ -> assert false

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
  | _, _ -> raise ProcessFailed

(* decompress + untar, "f" will read the output from bsdtar:
 *   'bsdtar xv -O' outputs the content of files to stdout
 *   'bsdtar xv' outputs the list of files expanded to stderr
 *   'bsdtar t' outputs the list of files to stdout *)
let from_tar action input =
  let t_r, t_w = U.pipe () in
  (* as per the comment before the function, we have to read stdout or stderr *)
  let t_args, t_stdout, t_stderr = match action with
  | `extract (pq, strip, iq) ->
      [| tar; "xvf"; input; "--strip-components"; strip |], U.stdout, t_w
  | `get file -> [| tar; "xf"; input; "-qO"; file |], t_w, U.stderr
  | `list -> [| tar; "tf"; input |], t_w, U.stderr
  in
  let t_pid = U.create_process t_args.(0) t_args U.stdin t_stdout t_stderr in
  let l = read t_pid t_r in
  List.iter U.close [ t_r; t_w ];
  l

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
  let () = output_string oc contents in
  let () = close_out oc in
  dir, base_name

(* reads 'package_script.el' from a package *)
let open_package package =
  let l = from_tar (`get "package_script.el") package in
  let s = String.concat "\n" l in
  script_of_sexp (Sexplib.Sexp.of_string s)

let rev_uniq l =
  let rec rev_uniq_rc accu cur = function
    | t :: q when t = cur -> rev_uniq_rc accu cur q
    | t :: q -> rev_uniq_rc (t :: accu) t q
    | [] -> accu
  in
  match l with
    | t :: q -> rev_uniq_rc [ t ] t q
    | [] -> []

let arch_of_preds predicates =
  try List.assoc "arch" predicates with Not_found -> "noarch"

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

