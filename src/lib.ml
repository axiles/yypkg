open Printf
open Sexplib
open Types

let install_path =
  Filename.dirname (Filename.basename (Sys.argv.(0)))

let ahk_bin =
  Filename.concat install_path "ahk.exe"

let db_path =
  Filename.concat install_path "db"

let rev_list_of_queue q =
  Queue.fold (fun l e -> e::l) [] q

let expand_environment_variables s =
  (REPLACE "${" (alnum+ as s) "}" -> Unix.getenv s) s

let quote_and_expand x =
  Filename.quote (expand_environment_variables x)

let reduce_path path =
  FilePath.DefaultPath.reduce path

let dir_sep =
  match Sys.os_type with
    | "Unix"
    | "Cygwin" -> '/'
    | "Win32" -> '\\'
    | _ -> assert false

let command cmd =
  let read_stdout s =
    let queue = Queue.create () in
    let ic = Unix.open_process_in s in
    let () = try
      while true do
        Queue.add (reduce_path (input_line ic)) queue
      done
    with End_of_file -> () in
    let () = ignore (Unix.close_process_in ic) in
    rev_list_of_queue queue
  in
  read_stdout cmd

(* let split_path path =
  let rec f path accu =
    match Filename.dirname path with
      | "."
      | "/"  -> (Filename.basename path) :: accu
      | s -> f s ( (Filename.basename path) :: accu)
  in
  match path with
    | "/" -> [ "/" ]
    | "." -> [ "." ]
    | _ -> f path []
*)

let split_path path =
  (* FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME *)
  (COLLECT [ ^ '/' ]+ as s -> s) path

let filename_concat = function
  | t :: q -> List.fold_left Filename.concat t q
  | [] -> raise (Invalid_argument "filename_concat, nothing to concat")

let chop_list list i =
  let rec chop_list_rc i = function
    | l when i = 0 -> l
    | t :: q -> chop_list_rc  (i-1) q
    | [] -> raise (Invalid_argument "chop_list, choping more than possible")
    (* this means we're trying to chop more than possible, 'l when i = 0'
     * handles the case when we're trying to chop as much as we have so we
     * can simply always yell here *)
  in
  chop_list_rc i list

let expand pkg i p =
  let l = (List.length (split_path i)) - 1 in
  let pkg = quote_and_expand pkg in
  let iq = quote_and_expand ("./" ^ i) in
  let pq = quote_and_expand p in
  let cmd= sprintf "tar xvf %s -C %s --strip-component %d %s" pkg pq (l+1) iq in
  let lines = command cmd in
  let actual_path path =
    filename_concat (p :: (chop_list (split_path path) l))
  in
  List.map actual_path lines

let mkdir path_unexpanded =
  let path = expand_environment_variables path_unexpanded in
  let () = FileUtil.StrUtil.mkdir ~parent:true ~mode:0o755 path in
  [ path_unexpanded ]

let rm path_unexpanded =
  let path = expand_environment_variables path_unexpanded in
  try
    FileUtil.StrUtil.rm ~force:FileUtil.Force ~recurse:false [ path ];
    Printf.printf "Removed: %s\n" path
  with
    | FileUtil.RmDirNotEmpty s ->
        Printf.printf "Not removed: non-empty directory %s\n" s

let open_package package =
  let script_cmd = Printf.sprintf "tar xf %s -O ./package_script.el" package in
  let script_input = Unix.open_process_in script_cmd in
  let script_sexp = Sexp.input_sexp script_input in
  script_of_sexp script_sexp

let list_map_bis f l =
  let rec list_map_bis_rc f accu = function
    | t :: q -> list_map_bis_rc f ((f q t)::accu) q
    | [] -> List.rev accu
  in
  list_map_bis_rc f [] l

let file_exists_in_package file (_, result_list) =
  let f = function
    | _, NA -> false
    | _, Filelist l -> List.exists ((=) file) l
  in
  List.exists f result_list

let files_from_package (_, results) =
  let f = function
    | NA -> []
    | Filelist l -> List.map reduce_path l
  in
  List.concat (List.map f results)
