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

(* Split a string "X=A,B,C" into (X, [A; B; C]) *)
let key_value_pair s =
  let l = String.length s in
  let i = String.index s '=' in
  let key = String.sub s 0 i in
  let value = Str.split (Str.regexp ",") (String.sub s (i+1) (l-i-1)) in
  key, value

(* updates the association list from "X=A,B,C" strings *)
let setpred conf pred =
  Conf.set conf (key_value_pair pred)

let delpred conf pred =
  Conf.unset conf pred

let set_mirror conf mirror =
  { conf with mirror }

type predicates = {
  list : bool;
  set : string list;
  delete : string list;
}

let predicates opts =
  let init = { list = false; set = []; delete = [] } in
  let l = [
    "--list", (fun ~accu n o ->
      { accu with list = Args.(get bool n o) });
    "--set", (fun ~accu n o ->
      { accu with set = Args.(get string n o) :: accu.set });
    "--delete", (fun ~accu n o ->
      { accu with delete = Args.(get string n o) :: accu.delete });
  ] in
  let o = Args.foo ~init ~where:"--predicates" l opts in
  match o.list, o.set, o.delete with
  | true, [], [] ->
      Conf.print_predicates (Conf.read ())
  | true, _, _ ->
      Lib.ep "WARNING: both --list and --(un)set given to --config; only --list is executed.\n%!";
      Conf.print_predicates (Conf.read ())
  | false, [], [] ->
      Lib.ep "WARNING: none of --list, --set, --unset given to --config.\n%!";
  | _, _, [] ->
      Conf.update (fun conf -> List.fold_left setpred conf o.set)
  | _, [], _ ->
      Conf.update (fun conf -> List.fold_left delpred conf o.delete)
  | _, _, _ ->
      Lib.ep "WARNING: both --set and --unset given to --config; doing nothing.\n%!"

let main opts =
  ListLabels.iter opts ~f:(function
  | Args.Opt ("--predicates", subopts) ->
      predicates opts
  | Args.Opt ("--set-mirror", [ Args.Val mirror ]) ->
      Conf.update (fun conf -> set_mirror conf mirror)
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
