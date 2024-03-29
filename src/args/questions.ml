module Path = struct
  let rec get ?(mkdir=(fun _ -> ())) ~existing =
    let s = read_line () in
    if String.length s = 0 then
      (print_endline "Path must not be empty."; get ~mkdir ~existing)
    else
      match existing, Sys.file_exists s with
      | true, true ->
          s
      | true, false ->
          print_endline "Path must currently exist.";
          get ~mkdir ~existing
      | false, true ->
          print_endline "Path must not currently exist.";
          get ~mkdir ~existing
      | false, false ->
          try mkdir s; s with e ->
            Printf.printf "Path couldn't be created: %s; please check it."
              (Printexc.to_string e);
            get ~mkdir ~existing
end

module Choice = struct
  let rec get sm =
    let choices = StringMatcher.possible_values ~t:sm in
    Printf.printf "Chose one of %s (case-insensitive).\n" choices;
    try StringMatcher.of_string ~t:sm (read_line ()) with
    | _ ->
        print_endline "Could not understand the answer.";
        get sm
end

module String = struct
  let rec get () =
    let s = read_line () in
    if String.length s = 0 then
      (print_endline "Answer cannot be empty."; get ())
    else
      s
end

