type 'a t = (string * string list * 'a) list

let possible_values ~t =
  String.concat ", " (List.map (fun (s, _, _) -> String.escaped s) t)

let of_string ~t s =
  let pred e = Str.string_match (Str.regexp_case_fold e) s 0 in
  let _, _, z = List.find (fun (x, y, _) -> List.exists pred (x :: y)) t in
  z

let to_string ~t e =
  let s, _, _ = List.find (fun (_, _, e_) -> e_ = e) t in
  s

let bool = [
  "yes", [ "true" ], true;
  "no", [ "false" ], false;
]
