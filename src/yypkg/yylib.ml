open Printf
open Sexplib
open Types

exception ChopList_ChopingTooMuch of (int * int)

let ahk_bin =
  Filename.concat Lib.install_dir "ahk.exe"

let db_path =
  "yypkg_db"

let rev_list_of_queue q =
  Queue.fold (fun l e -> e::l) [] q

(* replace env var of the form ${windir} *)
let expand_environment_variables s =
  Str.global_substitute (Str.regexp "\\${[0-9a-zA-Z]}") Unix.getenv s

(* bsdtar writes 'x some/path/foo' during extraction *)
let filter_bsdtar_output x =
  if x.[0] = 'x' && x.[1] = ' ' then
    String.sub x 2 (String.length x - 2)
  else
    x

let reduce_path path =
  FilePath.DefaultPath.reduce path

(* returns the list of lines in the in_channel ic *)
let read_ic ?(tar=false) pid ic =
  let descr = Unix.descr_of_in_channel ic in
  let read ic = if tar
    then reduce_path (filter_bsdtar_output (input_line ic))
    else reduce_path (input_line ic)
  in
  let rec f accu =
    match Unix.select [ descr ] [] [] 0.1 with
      | [ _ ], _, _ -> f ((read ic) :: accu)
      | _, _, _ -> if pid = fst (Unix.waitpid [ Unix.WNOHANG ] pid)
          then accu
          else f accu
  in
  f []

(* run the command cmd and return a list of lines of the output *)
let command cmd =
  (* XXX: pid! *)
  read_ic 0 (Unix.open_process_in cmd)

let split_path path =
  Str.split (Str.regexp Lib.dir_sep) path

(* List.fold_left Filename.concat *)
let filename_concat = function
  | t :: q -> List.fold_left Filename.concat t q
  | [] -> raise (Invalid_argument "filename_concat, nothing to concat")

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
let strip_component ?prefix n path =
  match prefix with
    | None -> filename_concat (chop_list (split_path path) n)
    | Some prefix -> filename_concat (prefix :: (chop_list (split_path path) n))

(* mkdir for use in installation scripts: it returns the path that got created
 * so it can be registered and reversed upon uninstallation *)
let mkdir path_unexpanded =
  let path = expand_environment_variables path_unexpanded in
  let () = FileUtil.mkdir ~parent:true ~mode:0o755 path in
  [ path_unexpanded ]

(* tar xf the folder 'i' in the package 'pkg' to the folder 'p' *)
let expand pkg i p =
  let l = List.length (split_path i) in
  let pkg = expand_environment_variables pkg in
  let iq = expand_environment_variables i in
  let pq = expand_environment_variables p in
  if not (Sys.file_exists p) then ignore (mkdir p) else ();
  let tar_args = Array.append 
    (* gnu tar doesn't default to --wildcards while bsdtar defaults to wildcards
     * and doesn't recognize the option *)
    ( if Lib.tar = "tar" then [| "--wildcards" |] else [| |] )
    [| "-C"; pq; "--strip-components"; string_of_int (l-1); iq |]
  in
  let x = Lib.decompress_untar (read_ic ~tar:true) tar_args pkg in
  List.map (strip_component ~prefix:p (l-1)) x

(* rm with verbose output
 *   doesn't fail if a file doesn't exist
 *   removes directories only if empty *)
let rm path_unexpanded =
  (* Sys.file_exists and most others will follow symlinks
   * This means that if 'y' points to 'x' but 'x' doesn't exist, Sys.file_exists
   * will return false even though 'y' exists *)
  let exists path =
    try let () = ignore (Unix.lstat path) in true with _ -> false
  in
  let path = expand_environment_variables path_unexpanded in
  if exists path then
    if Sys.is_directory path then
      if [| |] = Sys.readdir path then
        let () = FileUtil.rm ~recurse:true [ path ] in
        Printf.printf "Removed: %s\n" path
      else
        Printf.printf "Not removed (directory not empty): %s\n" path
    else
      let () = FileUtil.rm [ path ] in
      Printf.printf "Removed: %s\n" path
  else
    Printf.printf "Not removed (doesn't exist): %s\n" path

(* reads 'package_script.el' from a package *)
let open_package package =
  (* XXX: first arg to decompress_untar *)
  let script_sexp = Lib.decompress_untar (fun _ ic -> Sexp.input_sexp ic) [| "-O"; "package_script.el" |] package in
  script_of_sexp script_sexp

(* checks if a file exists in any package in a given database *)
let file_exists_in_package file (_, result_list) =
  let f = function
    | _, NA -> false
    | _, Filelist l -> List.exists ((=) file) l
  in
  List.exists f result_list

(* a predicate to check a package has some name, used with List.find *)
let package_is_named name ((m, _, _), _) =
  name = m.package_name
