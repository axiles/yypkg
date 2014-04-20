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
open Yylib

let p = Printf.printf

module Arch = struct
  let x86_64_is_available =
    try
      Sys.file_exists (Filename.concat (Sys.getenv "systemroot") "SysWOW64")
    with Not_found -> false

  let cli_spec =
    let mk ~n ~h c = Args.spec ~name:n ~help:h ~children:c in
    mk ~n:"--i686" ~h:"install for i686 (cumulative)" [] ::
      if x86_64_is_available then
        [ mk ~n:"--x86_64" ~h:"install for x86_64 (cumulative)" [] ]
      else
        []

  let get name = function
    | Some b -> b
    | None -> 
        Printf.printf "Do you want to install for %s?\n%!" name;
        Questions.Choice.get StringMatcher.bool
end

let host_spec = [
  "Cygwin", [], `Cygwin;
  "MSYS", [], `MSYS;
  "Native Windows", [ "Windows" ], `Windows;
]

let get_mirror = function
  | Some host -> host
  | None ->
      p "Which mirror do you want to use? (e.g. http://win-builds.org/1.4.0/)\n";
      Questions.String.get ()

let get_host = function
  | Some host -> host
  | None ->
      p "Which system do you want to install for?\n";
      let s = Questions.Choice.get host_spec in
      p "\n";
      s

let mirror () =
  let re =
    let base = "yypkg-\\([-._0-9a-zA-Z]+\\)" in
    let suf = if Lib.os_type = `Windows then "\\.exe" else "" in
    Str.regexp (base ^ suf ^ "$")
  in
  let executable = Filename.basename Sys.executable_name in
  if try Str.search_forward re executable 0 >= 0 with Not_found -> false then
    Some ("http://win-builds.org/" ^ (Str.matched_group 1 executable))
  else
    None

let install ~host ~mirror ~arch =
  let host_triplet, bits = match arch with
  | `I686 -> "i686-w64-mingw32", 32
  | `X86_64 -> "x86_64-w64-mingw32", 64
  in

  let mkdir x = ignore (mkdir x) in
  let prefix = match host with
  | `Windows -> 
      p "Where do you want to install win-builds %d? (environment variables of the form ${FOO} are understood)\n" bits;
      Questions.Path.get ~mkdir ~existing:false
  | `MSYS ->
      let opt_path = Lib.sp "/opt/windows_%d" bits in
      p "Please provide the full Windows path of your MSYS installation with forward-slashes, e.g. C:/MSYS (environment variables of the form ${FOO} are understood).\n";
      Filename.concat (Questions.Path.get ~mkdir ~existing:true) opt_path
  | `Cygwin ->
      let opt_path = Lib.sp "/opt/windows_%d" bits in
      try
        Lib.run_and_read [| "cygpath"; "-m"; opt_path |] `stdout
      with _ ->
        p "Could not find the path to the Cygwin installation.\n";
        p "Please provide the full Windows path of your Cygwin installation with forward-slashes, e.g. C:/Cygwin (environment variables of the form ${FOO} are understood).\n";
        Filename.concat (Questions.Path.get ~mkdir ~existing:true) opt_path
  in

  let host_system = StringMatcher.to_string ~t:host_spec host in

  p "\nInstalling win-builds %d in %S for %S.\n"
    bits
    prefix
    host_system;
  p "\nPress return to continue or Ctrl-C to abort.\n";
  ignore (read_line ());

  Init.init prefix;
  let conf = Config.update (fun conf ->
    let p_set = Config.Predicates.set in
    let conf = p_set conf ("host", [ host_triplet ]) in
    let conf = p_set conf ("target", [ host_triplet ]) in
    let conf = p_set conf ("host_system", [ host_system ]) in
    { conf with mirror = Lib.sp "%s/packages/windows_%d" mirror bits }
  )
  in
  let l = Web.packages ~conf ~follow:true ~wishes:["all"] in
  let packages = Web.download ~conf ~dest:Yylib.default_download_path l in
  Db.update (Install.install conf packages)

type deploy_opts = {
  mirror : string option;
  host : [ `Cygwin | `MSYS | `Windows ] option;
  i686 : bool option;
  x86_64 : bool option;
}

let main opts =
  let init = {
    mirror = mirror ();
    host = None;
    i686 = if not Arch.x86_64_is_available then Some true else None;
    x86_64 = if not Arch.x86_64_is_available then Some false else None;
  } in
  let l = [
    "--mirror", (fun ~accu n o ->
      { accu with mirror = Some (Args.Get.string n o) });
    "--i686", (fun ~accu n o ->
      { accu with i686 = Some (Args.Get.bool n o) });
    "--x86_64", (fun ~accu n o ->
      { accu with x86_64 = Some (Args.Get.bool n o) });
    "--host", (fun ~accu n o ->
      { accu with host = Some (Args.Get.of_stringmatcher host_spec n o) });
  ] in
  let o = Args.fold_values ~init ~where:"--deploy" l opts in
  let mirror = get_mirror o.mirror in
  p "Using mirror %S.\n\n" mirror;
  let host = get_host o.host in
  let i686 = Arch.get "i686" o.i686 in
  let x86_64 = Arch.get "x86_64" o.x86_64 in

  (if i686 then install ~host ~mirror ~arch:`I686);
  (if x86_64 then install ~host ~mirror ~arch:`X86_64)

let cli_spec =
  let mk ~n ~h c = Args.spec ~name:n ~help:h ~children:c in
  mk ~n:"--deploy" ~h:"parent option for:" (
    ( mk ~n:"--host" ~h:"one of \"MSYS\", \"Cygwin\", \"Windows\"" [])
    :: Arch.cli_spec
  )
