open Types
  
open Yylib
  
let execute_install_action package (id, action) =
  match action with
  | AHK p -> (id, (Filelist (command (String.concat " " (ahk_bin :: p)))))
  | (* quote *) Expand (i, p) -> (id, (Filelist (expand package i p)))
  | Exec p -> (id, (Filelist (command (String.concat " " p))))
  | (* quote *) MKdir p -> (id, (Filelist (mkdir p)))
  
let install_package package (conf : predicates) db =
  let (((metadata, install_actions, _) as script)) = open_package package in
  let (_, f_preds) =
    List.partition (predicate_holds conf) metadata.predicates
  in
    if 0 = (List.length f_preds)
    then
      (let results =
         List.map (execute_install_action package) install_actions in
       let updated_db = Db.install_package db (script, (List.rev results))
       in updated_db)
    else raise (Unmatched_predicates f_preds)
  
let install p conf db =
  let p = FilePath.DefaultPath.make_absolute Lib.install_path p
  in
    ((* check the file exists, may raise 'File_not_found p' : caught in yypkg.ml *)
     assert_file_exists p;
     (* if the line above didn't abort, go on and instal the package *)
     install_package p conf db)
  

