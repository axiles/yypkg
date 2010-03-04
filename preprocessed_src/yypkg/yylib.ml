open Printf
  
open Sexplib
  
open Types
  
exception ChopList_ChopingTooMuch of (int * int)
  
let ahk_bin = Filename.concat install_dir "ahk.exe"
  
let db_path = "yypkg_db"
  
let rev_list_of_queue q = Queue.fold (fun l e -> e :: l) [] q
  
let expand_environment_variables s =
  Str.global_substitute (Str.regexp "\\${[0-9a-zA-Z]}") Unix.getenv s
  
let quote_and_expand x = expand_environment_variables x
  
let filter_bsdtar_output x =
  if (x.[0] = 'x') && (x.[1] = ' ')
  then String.sub x 2 ((String.length x) - 2)
  else x
  
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
  
let read_ic ic =
  let queue = Queue.create () in
  let () =
    try
      while true do
        Queue.add (reduce_path (filter_bsdtar_output (input_line ic))) queue
        done
    with | End_of_file -> ()
  in rev_list_of_queue queue
  
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
  let pq = quote_and_expand p
  in
    (if not (Sys.file_exists p) then ignore (mkdir p) else ();
     let tar_args =
       if Lib.tar = "tar"
       then
         [| "--wildcards"; "-C"; pq; "--strip-components";
           string_of_int (l - 1); iq
         |]
       else [| "-C"; pq; "--strip-components"; string_of_int (l - 1); iq |] in
     let x = Lib.decompress_untar read_ic tar_args pkg
     in List.map (strip_component ~prefix: p (l - 1)) x)
  
let rm path_unexpanded =
  let exists path =
    try let () = ignore (Unix.lstat path) in true with | _ -> false in
  let path = expand_environment_variables path_unexpanded
  in
    if exists path
    then
      if Sys.is_directory path
      then
        if [|  |] = (Sys.readdir path)
        then
          (let () = FileUtil.rm ~recurse: true [ path ]
           in Printf.printf "Removed: %s\n" path)
        else Printf.printf "Not removed (directory not empty): %s\n" path
      else
        (let () = FileUtil.rm [ path ] in Printf.printf "Removed: %s\n" path)
    else Printf.printf "Not removed (doesn't exist): %s\n" path
  
let open_package package =
  let script_sexp = Lib.decompress_untar Sexp.input_sexp [| "-O" |] package
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
  

