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
      p "Where do you want to install win-builds %d?\n" bits;
      p "The installation will create the directories bin, include, lib and others under this location.\n";
      p "(environment variables of the form ${FOO} are understood).\n";
      Questions.Path.get ~mkdir ~existing:false
  | `MSYS -> (
      let opt_path = Lib.sp "/opt/windows_%d" bits in
      let q () =
        p "Couldn't automatically find the MSYS installation path.\n";
        p "Please provide it in full as a Windows path with forward-slashes.\n";
        p "For example C:/MinGW/msys/1.0; toolchain will be put in C:/MSYS%s.\n" opt_path;
        p "(environment variables of the form ${FOO} are understood).\n";
        Filename.concat (Questions.Path.get ~mkdir ~existing:true) opt_path
      in
      try
        let path = Filename.dirname (Sys.getenv "WD") in
        if Sys.file_exists path then
          let msys_path = Str.global_replace (Str.regexp "\\\\") "/" path in
          Filename.concat msys_path opt_path
        else
          q ()
      with _ ->
        q ()
    )
  | `Cygwin ->
      let opt_path = Lib.sp "/opt/windows_%d" bits in
      try
        let s = Lib.run_and_read [| "cygpath"; "-m"; opt_path |] `stdout in
        Str.global_replace (Str.regexp "\n$") "" s
      with _ ->
        p "Couldn't automatically find Cygwin installation path.\n";
        p "Please provide it in full as a Windows path with forward-slashes.\n";
        p "For example, C:/Cygwin. Toolchain will be put under %s.\n" opt_path;
        p  "(environment variables of the form ${FOO} are understood).\n";
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
  (* FIXME: prefix might not be an absolute path *)
  Unix.putenv "YYPREFIX" prefix;
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
  mirror_ : string option;
  host_ : [ `Cygwin | `MSYS | `Windows ] option;
  i686 : bool option;
  x86_64 : bool option;
}

let press_return_to_exit () =
  if Lib.started_from_windows_gui () then (
    print_endline "Installation done. Press return to continue...";
    ignore (read_line ())
  )
  else
    ()

let main opts =
  let init = {
    mirror_ = mirror ();
    host_ = None;
    i686 = if not Arch.x86_64_is_available then Some true else None;
    x86_64 = if not Arch.x86_64_is_available then Some false else None;
  } in
  let l = [
    "--mirror", (fun ~accu n o ->
      { accu with mirror_ = Some (Args.Get.string n o) });
    "--i686", (fun ~accu n o ->
      { accu with i686 = Some (Args.Get.bool n o) });
    "--x86_64", (fun ~accu n o ->
      { accu with x86_64 = Some (Args.Get.bool n o) });
    "--host", (fun ~accu n o ->
      { accu with host_ = Some (Args.Get.of_stringmatcher host_spec n o) });
  ] in
  let o = Args.fold_values ~init ~where:"--deploy" l opts in
  let mirror = get_mirror o.mirror_ in
  p "Using mirror %S.\n\n" mirror;
  let host = get_host o.host_ in
  let i686 = Arch.get "i686" o.i686 in
  let x86_64 = Arch.get "x86_64" o.x86_64 in

  try
    (if i686 then install ~host ~mirror ~arch:`I686);
    (if x86_64 then install ~host ~mirror ~arch:`X86_64);
    press_return_to_exit ()
  with _ -> press_return_to_exit ()

let cli_spec =
  let mk ~n ~h c = Args.spec ~name:n ~help:h ~children:c in
  mk ~n:"--deploy" ~h:"parent option for:" (
    (mk ~n:"--host" ~h:"one of \"MSYS\", \"Cygwin\", \"Windows\"" [])
    :: Arch.cli_spec
  )
