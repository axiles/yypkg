(* splits the string "A=X" into "A","X" and then updates the association list *)
let setpred conf pred =
  let key_value_pair s =
    let l = String.length s in
    let i = String.index s '=' in
    let key = String.sub s 0 i in
    let value = Str.split (Str.regexp ",") (String.sub s (i+1) (l-i-1)) in
    key, value
  in
  let pred = key_value_pair pred in
  Conf.set conf pred

let delpred conf pred =
  Conf.unset conf pred

(* Nothing to do right now *)
(* let regen _ = () *)
