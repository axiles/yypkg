module Path = struct
  let rec get ?(mkdir=(fun _ -> ())) ~existing =
    let s = read_line () in
    if String.length s = 0 then
      (print_endline "Path must not be empty."; get ~mkdir ~existing)
    else
      match existing, Sys.file_exists s with
      | true, true -> s
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
    Printf.printf "Chose one of %s.\n" (StringMatcher.possible_values ~t:sm);
    try StringMatcher.of_string ~t:sm (read_line ()) with
    | _ ->
        print_endline "Could not understand the answer.";
        get sm
end

