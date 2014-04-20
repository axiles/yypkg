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

open Types

module Predicates = struct
  let set conf (binding, value) =
    let predicates = List.remove_assoc binding conf.preds in
    { conf with preds = (binding, value) :: predicates }

  let unset conf binding =
    { conf with preds = List.remove_assoc binding conf.preds }
end

let read () =
  TypesSexp.To.conf (Disk.read Yylib.conf_path)

let write conf =
  (* Let's sort the predicates. Won't be faster but should be nicer to read
   * when editing the file by hand. It'll also avoid requiring to sort the
   * output when listing the configuration to the user.
   * We use stable_sort so not to change anything if there are several bindings
   * for the same value. *)
  let conf = { conf with preds = List.stable_sort compare conf.preds } in
  Disk.write Yylib.conf_path (TypesSexp.Of.conf conf)

(* read the conf, run the function, write the database to disk
 * if fail raises an exception, nothing will be written :-) *)
let update f =
  let c = f (read ()) in
  write c;
  c

let print_predicates conf =
  let print_single_pred (binding, values) =
    Printf.printf "%s = %s\n" binding (String.concat "," values)
  in
  List.iter print_single_pred conf.preds

(* Split a string "X=A,B,C" into (X, [A; B; C]) *)
let key_value_pair s =
  let l = String.length s in
  let i = String.index s '=' in
  let key = String.sub s 0 i in
  let value = Str.split (Str.regexp ",") (String.sub s (i+1) (l-i-1)) in
  key, value

type predicates = {
  list : bool;
  set : string list;
  delete : string list;
}

let predicates opts =
  let init = { list = false; set = []; delete = [] } in
  let l = [
    "--list", (fun ~accu n o ->
      { accu with list = Args.Get.bool n o });
    "--set", (fun ~accu n o ->
      { accu with set = Args.Get.string n o :: accu.set });
    "--delete", (fun ~accu n o ->
      { accu with delete = Args.Get.string n o :: accu.delete });
  ] in
  let o = Args.fold_values ~init ~where:"--predicates" l opts in
  match o.list, o.set, o.delete with
  | true, [], [] ->
      print_predicates (read ())
  | true, _, _ ->
      Lib.ep "WARNING: both --list and --(un)set given to --config; only --list is executed.\n%!";
      print_predicates (read ())
  | false, [], [] ->
      Lib.ep "WARNING: none of --list, --set, --unset given to --config.\n%!";
  | _, _, [] ->
      (* updates the association list from "X=A,B,C" strings *)
      ignore (update (fun conf -> List.fold_left (fun conf p -> Predicates.set conf (key_value_pair p)) conf o.set))
  | _, [], _ ->
      ignore (update (fun conf -> List.fold_left Predicates.unset conf o.delete))
  | _, _, _ ->
      Lib.ep "WARNING: both --set and --unset given to --config; doing nothing.\n%!"

let main opts =
  ListLabels.iter opts ~f:(function
  | Args.Opt ("--predicates", subopts) ->
      predicates opts
  | Args.Opt ("--set-mirror", [ Args.Val mirror ]) ->
      ignore (update (fun conf -> { conf with mirror }))
  | Args.Opt ("--set-mirror", _) ->
     raise (Args.Parsing_failed "--set-mirror requires a string as argument.")
  | Args.Opt (name, _) ->
     raise (Args.Parsing_failed (Lib.sp "Unknown argument `%s'." name))
  | Args.Val v ->
     raise (Args.Parsing_failed (Lib.sp "--config requires a sub-option, not `%s'." v))
  )

let cli_spec =
  let mk ~n ~h c = Args.spec ~name:n ~help:h ~children:c in
  mk ~n:"--config" ~h:"parent option for:" [
    mk ~n:"--predicates" ~h:"" [
      mk ~n:"--set" ~h:"set predicates (e.g. \"host=x86_64-w64-mingw32\")" [];
      mk ~n:"--delete" ~h:"delete predicates" [];
      mk ~n:"--list" ~h:"list predicates" [];
    ];
    mk ~n:"--set-mirror" ~h:"set the mirror to use" [];
    (* add --print *)
  ];
