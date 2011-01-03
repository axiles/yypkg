let code oc image link = Printf.fprintf oc "\
<body>
  <a href=\"..\">Up</a><br/>
  <a href=\"%s\"><img src=\"%s\"></a>
</body>" link image

let print image ?(link = "index") () =
  let oc = open_out_bin (image ^ ".html") in
  code oc (image ^ ".png") (link ^ ".html");
  close_out oc

let rec f = function
  | s :: (p :: _ as q) -> print s ~link:p (); f q
  | s :: [] -> print s ()
  | [] -> ()

let symlink s d =
  (if Sys.file_exists d then Unix.unlink d);
  Unix.symlink s d

let () =
  let files = Sys.readdir "." in
  let files = Array.to_list files in
  let files = List.filter (fun s -> Filename.check_suffix s ".png") files in
  let files = List.map (fun s -> Filename.chop_suffix s ".png") files in
  f files;
  try symlink ((List.hd files) ^ ".html") "index.html" with Failure "hd" -> ()

