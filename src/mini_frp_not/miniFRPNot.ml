type 'a t = ('a -> unit) ref

let create name =
  let s = name ^ ": not initialized.\n" in
  ref (fun _ -> prerr_endline s)

let fold func init t =
  let state = ref init in
  t := (fun v -> state := func !state v)
