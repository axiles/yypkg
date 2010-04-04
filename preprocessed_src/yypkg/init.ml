(* Init a yypkg installation in the given folder, this means:
  * create /etc and an empty conf in /etc/yypkg.conf
  * create /sbin
  * create /var/log/packages and an empty db in /var/log/packages/yypkg_db
  * put the required binaries in /sbin (xz, gzip, bzip2, (bsd)tar...) *)
open Types
  
open Yylib
  
let mkdir = FileUtil.mkdir ~parent: true ~mode: 0o755
  
let init () =
  let folders = [ [ "etc" ]; [ "sbin" ]; [ "var"; "log"; "packages" ] ]
  in
    (List.iter (fun l -> mkdir (filename_concat l)) folders;
     Disk.write db_path (sexp_of_db []);
     Disk.write conf_path (sexp_of_conf []))
  

