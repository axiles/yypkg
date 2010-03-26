(* Init a yypkg installation in the given folder, this means:
  * create /etc (will hold /etc/yypkg.conf)
  * create /sbin
  * create /var/log/packages (will hold the package database)
  * put the required binaries in /sbin (xz, gzip, bzip2, (bsd)tar...) *)

open Types
open Yylib

let mkdir =
  FileUtil.mkdir ~parent:true ~mode:0o755

let init () =
  let folders = [
    [ "etc" ];
    [ "sbin" ];
    [ "var"; "log"; "packages" ];
  ]
  in
  List.iter (fun l -> mkdir (filename_concat l)) folders
