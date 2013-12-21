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

let cmd_line_spec =
  let mk ~n ~h c = Args.spec ~name:n ~help:h ~children:c in
  [
    mk ~n:"-prefix" ~h:"prefix sherpa will be working in" [];
    mk ~n:"-follow-dependencies" ~h:"also fetch and install dependencies" [];
    mk ~n:"-download-folder"
      ~h:("download files there (instead of " ^ Yylib.default_download_path ^ ")") [];
    mk ~n:"-set-mirror" ~h:"set the mirror to use" [];
    mk ~n:"-install" ~h:"install packages" [];
    mk ~n:"-download" ~h:"download packages" [];
]

let settings_of_cmd_line ~start_dir cmd_line =
  let pred x = List.exists (fun s -> Args.is_opt ~s x) [
    "-follow-dependencies"; "-download-folder"] in
  let lt, lf = List.partition pred cmd_line in
  let follow = List.exists (Args.is_opt ~s:"-follow-dependencies") lt in
  let dest =
    try
      match List.find (Args.is_opt ~s:"-download-folder") lt with
      | Args.Opt (_, [ Args.Val dest ]) -> FilePath.make_absolute start_dir dest
      | Args.Opt _ -> raise (Args.Parsing_failed "Bad download folder")
      | _ -> assert false
    with Not_found ->
      FilePath.make_absolute (Sys.getcwd ()) Yylib.default_download_path
  in
  (follow, dest), lf

let main () =
  let b = Buffer.create 100 in
  if Args.wants_help () || Args.nothing_given () then (
    Args.bprint_spec b 0 (Args.usage_msg cmd_line_spec "sherpa");
    Buffer.output_buffer stderr b
  )
  else
    let cmd_line = Args.parse cmd_line_spec Sys.argv in
    let start_dir = Sys.getcwd () in
    let prefix, cmd_line = Yylib.prefix_of_cmd_line cmd_line in
    Sys.chdir prefix;
    Yylib.sanity_checks ();
    let (follow, dest), cmd_line = settings_of_cmd_line ~start_dir cmd_line in
    let action, actionopts = Yylib.action_of_cmd_line cmd_line in
    let opts = Args.to_string_list actionopts in
    let sherpa_conf = Sherpalib.read () in
    let yypkg_conf = Conf.read () in
    match action with
    | None -> ()
    | Some "-install" ->
        let packages = Sherpalib.get_packages ~sherpa_conf ~yypkg_conf ~follow
          ~dest ~packages:opts in
        Db.update (Install.install yypkg_conf packages)
    | Some "-download" ->
        ignore (Sherpalib.get_packages ~sherpa_conf ~yypkg_conf ~follow
        ~dest ~packages:opts)
    | Some "-set-mirror" -> begin
        match opts with
        | [ mirror ] -> Sherpalib.update (fun c -> { c with SherpaT.mirror })
        | [] -> raise (Args.Parsing_failed "No mirror given.")
        | _ -> raise (Args.Parsing_failed "Several mirrors given.")
      end
    | _ -> assert false

let () =
  main ()
