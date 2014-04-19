module Get = struct
  let to_file ~agent ~file ~uri =
    let fd = Unix.(openfile file [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644) in
    let out () =
      let t = ref (Unix.gettimeofday ()) in
      fun ~string ~offset ~length ->
        let t' = Unix.gettimeofday () in
        (if t' >= !t +. 1. then (prerr_char '.'; flush stderr; t := t'));
        ignore (Unix.write fd string offset length)
    in
    (try
      ignore (Http_get.body ~agent ~uri ~out:(out ()))
    with exn ->
      Unix.close fd; raise exn);
    Unix.close fd

  let to_string ~agent ?(b_size = 32*1024) uri =
    let b = Buffer.create b_size in
    let out () =
      let t = ref (Unix.gettimeofday ()) in
      fun ~string ~offset ~length ->
        let t' = Unix.gettimeofday () in
        (if t' >= !t +. 1. then (prerr_char '.'; flush stderr; t := t'));
        Buffer.add_substring b string offset length
    in
    ignore (Http_get.body ~agent ~uri ~out:(out ()));
    Buffer.contents b
end

let () =
  prerr_endline (Get.to_string ~agent:"lapin" "http://notk.org/~adrien/debug.txt")

