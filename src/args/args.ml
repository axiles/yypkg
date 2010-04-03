(* gallium's Arg module is quite frustrating: it's simple but also quite limited
 * This module tries to provide a nicer higher-level interface for argument
 * parsing
 * Among others, we're making it easy to have mandatory arguments and mandatory
 * and forbidden combinations of arguments.
 *)

(* '-mainopt1 -opt1subopt1 -opt1subopt2 -mainopt2' will be parsed as:
  * [ "mainopt1", [ "opt1subopt1"; "opt2subopt2"] ; "mainopt2", [] ] *)

(* Option_specification_is_ambiguous means the spec used to parse the params is
 * bad: a single argument can be matched by several elements of the spec
 * This isn't a fail of this library, the problem really is the spec *)
type spec = (string * spec * string) list

(* Something on the command-line is either an option (starts with a dash), or a
 * value. Values are free-form, options are checked against the spec. *)
type opt = 
  | Val of string
  | Opt of (string * opt list)

exception Option_specification_is_ambiguous
exception Incomplete_parsing of (opt list * string list)

(* at any point, we read the argument, if it starts with a '-' and is among the
 * options recognized, we store it as an option, if it's not recognized, we
 * complain. *)

(* is the string 's' one of the options recognized in 'opts'? *)
let opt_of_string opts s =
  (* currently, the option on the command-line has to match exactly the spec *)
  (* we *may* recognize '-foo:x=42:y=43' but '-foo x=42 y=43' does the same and
   * is already working *)
  let pred x (y, _, _) = y = x in
  (* we return the opt which is matching the string
   * if several ones match, we fail *)
  match List.find_all (pred s) opts with
    (* nothing found, option isn't recognized *)
    | [] -> raise Not_found
    (* one and only one element, option is recognized *)
    | [ o ] -> o
    (* if several options are matched, this means the option specification given
     * is bad *)
    | _ -> raise Option_specification_is_ambiguous

let rec parse (opts : spec) accu = function
  | ( t :: q ) as l when t.[0] = '-' -> begin
      try 
        let _, subopts, _ = opt_of_string opts t in
        let subs, q' = parse subopts [] q in
        parse opts (Opt (t, List.rev subs) :: accu) q'
      with Not_found -> accu, l
    end
  | t :: q -> parse opts (Val t :: accu) q
  | [] -> List.rev accu, []

let parse opts args =
  match parse opts [] (Array.to_list args) with
    | opts, [] -> opts
    | opts, q -> raise (Incomplete_parsing (opts, q))
  
(* this is a little test:
  let spec = [ "-install", []; "-uninstall", []]
  let _ = parse spec [| "-install"; "foo"; "-uninstall" -] *)

(* another one:
  let spec = [
  "-install", [];
  "-uninstall", [];
  "-list", [];
  "-config", [
    "-preds", [];
    "-regen", [];
  ]
  in
  parse spec [| "-install"; "a"; "b"; "c"; "-config"; "x"; "-preds"; "y";
  "-uninstall"; "d"; "e" |]
*)


let is_opt ?s = function
  | Val _ -> false
  | Opt (s', _) -> begin
      match s with
        | Some s -> s = s'
        | None -> true
    end

let val_of_opts = function
  | Opt _ -> assert false
  | Val s -> s

