(*
 * yypkg - A cross-platform package manager
 * Copyright (C) 2010-2014 Adrien Nader
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(* gallium's Arg module is quite frustrating: it's simple but also quite limited
 * This module tries to provide a nicer higher-level interface for argument
 * parsing
 * Among others, we're making it easy to have mandatory arguments and mandatory
 * and forbidden combinations of arguments.
 *)

(* '-mainopt1 -opt1subopt1 -opt1subopt2 -mainopt2' will be parsed as:
  * [ "mainopt1", [ "opt1subopt1"; "opt2subopt2"] ; "mainopt2", [] ] *)

type child = {
  name : string;
  children : child list;
  help : string;
}
and spec = child list

(* Something on the command-line is either an option (starts with two dashes),
 * or a value. Values are free-form, options are checked against the spec. *)
type opt = 
  | Val of string
  | Opt of (string * opt list)

let spec ~name ~children ~help = { name; children; help }

(* Option_specification_is_ambiguous means the spec used to parse the params is
 * bad: a single argument can be matched by several elements of the spec
 * This isn't a fail of this library, the problem really is the spec *)
exception Option_specification_is_ambiguous
exception Incomplete_parsing of (opt list * string list)
exception Parsing_failed of string

let rec bprint_spec b n { name; children; help } =
  Printf.bprintf b "%s%s : %s\n" (String.make n ' ') name help;
  List.iter (bprint_spec b (n+1)) children

(* at any point, we read the argument, if it starts with a '-' and is among the
 * options recognized, we store it as an option, if it's not recognized, we
 * complain. *)

(* is the string 's' one of the options recognized in 'opts'? *)
let opt_of_string opts s =
  (* currently, the option on the command-line has to match exactly the spec *)
  (* we *may* recognize '-foo:x=42:y=43' but '-foo x=42 y=43' does the same and
   * is already working *)
  let pred x { name } = name = x in
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

let rec parse opts accu = function
  (* starts with a dash, it's an option, maybe a valid one *)
  | ( t :: q ) as l when t.[0] = '-' && t.[1] = '-' -> begin
      try 
        (* what are the corresponding suboptions? *)
        let { children = subopts } = opt_of_string opts t in
        (* we'll try to parse as much of the *sub*options before returning to
         * the current level *)
        let subs, q' = parse subopts [] q in
        parse opts (Opt (t, List.rev subs) :: accu) q'
      (* this is raised in case the current option isn't valid: we stop the
       * current parsing and go up one level where the option might be valid *)
      with Not_found -> accu, l
    end
  (* does not start with a dash, it's a value *)
  | t :: q -> parse opts (Val t :: accu) q
  | [] -> List.rev accu, []

let parse spec args =
  match parse spec [] (Array.to_list args) with
  | opts, [] -> opts
  | opts, q -> raise (Incomplete_parsing (opts, q))

let wants_help () = 
  let argv = Array.to_list Sys.argv in
  [] = argv || List.exists (ListLabels.mem ~set:[ "-help"; "--help";  "-h" ]) argv

let nothing_given () =
  1 = Array.length Sys.argv

let usage_msg spec what =
  { name = "Usage"; children = spec; help = "command-line arguments to " ^ what}

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


(* returns true if the value given is an option which name matches the optional
 * argument ?s, if given. *)
let is_opt ?s = function
  | Val _ -> false
  | Opt (s', _) -> begin
      match s with
      | Some s -> s = s'
      | None -> true
    end

let is_val = function
  | Val _ -> true
  | _ -> false

let val_of_opts = function
  | Val s -> s
  | Opt _ -> assert false

let to_string_list l =
  let l, m = List.partition is_val l in
  if m <> [] then
    raise (Invalid_argument "to_string_list")
  else
    List.rev (List.rev_map val_of_opts l)

type 'a getter = (string -> 'a) * string

let bool = bool_of_string, "true or false"
let string = (fun x -> x), "a string"

let get getter name opt =
  let sp = Printf.sprintf in
  let fail name valid issue =
    let msg = sp "%s requires %s, not %s." name valid issue in
    raise (Invalid_argument msg)
  in
  match getter, opt with
  | (_, valid), Opt _ ->
      fail name valid "switches"
  | (f, valid), Val opt ->
      (try f opt with Invalid_argument _ -> fail name valid opt)

let foo ~where ~init l opts =
  let sp = Printf.sprintf in
  let fail msg =
    raise (Parsing_failed msg)
  in
  ListLabels.fold_left opts ~init ~f:(fun accu a ->
    match a with
    | Opt (o, v) ->
        let f = (try List.assoc o l with
        | Not_found -> fail (sp "Unknown option `%s' to %s." o where))
        in
        List.fold_left (fun accu v -> f ~accu o v) accu v
    | Val v ->
        fail (sp "%s requires a sub-option, not `%s'." where v))

