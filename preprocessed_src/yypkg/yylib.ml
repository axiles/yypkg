open Printf
  
open Sexplib
  
open Types
  
exception ChopList_ChopingTooMuch of (int * int)
  
let install_path = Sys.getcwd ()
  
let ahk_bin = Filename.concat install_path "ahk.exe"
  
let db_path = "yypkg_db"
  
let rev_list_of_queue q = Queue.fold (fun l e -> e :: l) [] q
  
let expand_environment_variables s =
  let s = Str.global_substitute (Str.regexp "\\${[0-9a-zA-Z]}") Unix.getenv s
  in Str.replace_first (Str.regexp "[a-zA-Z]:\\") "" s
  
let quote_and_expand x = Filename.quote (expand_environment_variables x)
  
let reduce_path path = FilePath.DefaultPath.reduce path
  
let command cmd =
  let read_stdout s =
    let queue = Queue.create () in
    let ic = Unix.open_process_in s in
    let () =
      try while true do Queue.add (reduce_path (input_line ic)) queue done
      with | End_of_file -> () in
    let () = ignore (Unix.close_process_in ic) in rev_list_of_queue queue
  in read_stdout cmd
  
let split_path path = Str.split (Str.regexp Lib.dir_sep) path
  
let filename_concat =
  function
  | t :: q -> List.fold_left Filename.concat t q
  | [] -> raise (Invalid_argument "filename_concat, nothing to concat")
  
let chop_list list i =
  let rec chop_list_rc j =
    function
    | l when j = 0 -> l
    | t :: q -> chop_list_rc (j - 1) q
    | [] -> raise (ChopList_ChopingTooMuch (List.length list, i))
  in
    (* this means we're trying to chop more than possible, 'l when i = 0'
     * handles the case when we're trying to chop as much as we have so we
     * can simply always yell here *)
    chop_list_rc i list
  
let strip_component ?(prefix = "") n path =
  match prefix with
  | "" -> filename_concat (chop_list (split_path path) n)
  | prefix -> filename_concat (prefix :: (chop_list (split_path path) n))
  
let mkdir path_unexpanded =
  let path = expand_environment_variables path_unexpanded in
  let () = FileUtil.mkdir ~parent: true ~mode: 0o755 path
  in [ path_unexpanded ]
  
let expand pkg i p =
  let l = List.length (split_path i) in
  let () = Printf.printf "%d : %s\n%!" l i in
  let pkg = quote_and_expand pkg in
  let iq = quote_and_expand i in
  let pq = quote_and_expand p in
  let () = ignore (mkdir p) in (* XXX let p2 :: _ = split_path p in *)
  let cmd =
    sprintf "%s xvf %s --wildcards -C %s --strip-component %d %s" Lib.tar pkg
      pq (l - 1) iq in
  let () = print_endline cmd
  in List.map (strip_component ~prefix: p (l - 1)) (command cmd)
  
let rm path_unexpanded =
  let path = expand_environment_variables path_unexpanded in
  let exists path =
    try let () = ignore (Unix.lstat path) in true with | _ -> false
  in
    try
      if exists path
      then
        (let () = FileUtil.rm ~force: FileUtil.Force [ path ]
         in Printf.printf "Removed: %s\n" path)
      else Printf.printf "Not removed (doesn't exist): %s\n" path
    with
    | FileUtil.RmDirNotEmpty s ->
        Printf.printf "Not removed: directory not-empty %s\n" s
    | e -> (print_endline "pouet"; raise e)
  
let open_package package =
  let script_cmd =
    sprintf "tar xf %s -O --occurrence=1 package_script.el" package in
  let script_input = Unix.open_process_in script_cmd in
  let script_sexp = Sexp.input_sexp script_input
  in script_of_sexp script_sexp
  
let list_map_bis f l =
  let rec list_map_bis_rc f accu =
    function
    | t :: q -> list_map_bis_rc f ((f q t) :: accu) q
    | [] -> List.rev accu
  in list_map_bis_rc f [] l
  
let file_exists_in_package file (_, result_list) =
  let f =
    function
    | (_, NA) -> false
    | (_, Filelist l) -> List.exists (( = ) file) l
  in List.exists f result_list
  
let files_from_package (_, results) =
  let f = function | NA -> [] | Filelist l -> List.map reduce_path l
  in List.concat (List.map f results)
  
let package_is_named name ((m, _, _), _) = name = m.package_name
  
