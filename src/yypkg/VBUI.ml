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

module ReturnCode = struct
  let ok = 1
  let cancel = 2
  let abort = 3
  let retry = 4
  let ignore = 5
  let yes = 6
  let no = 7
end

module Button = struct
  let ok = "vbOK"
  let cancel = "vbCancel"
  let abort = "vbAbort"
  let retry = "vbRetry"
  let ignore = "vbIgnore"
  let yes = "vbYes"
  let no = "vbNo"
end

let run s =
  let path, oc = Filename.open_temp_file "yy_vb_ui_" ".vbs" in
  output_string oc "res = ";
  output_string oc s;
  output_string oc "\nWScript.Quit res\n";
  close_out oc;
  let ret = Sys.command (Lib.sp "cscript.exe %S" path) in
  Unix.unlink path;
  ret

let msgbox ?(title = "Question") ~buttons text =
  run (Lib.sp "MsgBox (%S, %s, %S)" text (String.concat " | " buttons) title)

let main () =
  let conf = Config.read () in
  let db = Db.read () in
  let l = Web.packages ~conf ~follow:false ~wishes:[] in
  match List.filter (Web.needs_update ~db) l with
  | [] ->
      ignore (msgbox
        ~title:"No update available"
        ~buttons:[ Button.ok ]
        "There is no package update available")
  | l ->
      let summary =
        String.concat "\n" (ListLabels.map l ~f:(fun p ->
          let version, build = p.metadata.version in
          Lib.sp "%s: %s-%d" p.metadata.name version build))
      in
      let ret = msgbox
        ~title:"Update available"
        ~buttons:Button.([ yes; no ])
        (Lib.sp "There are %d packages to update: %s" (List.length l) summary)
      in
      if ret = ReturnCode.ok then (
        let packages = Web.download ~conf ~dest:Yylib.default_download_path l in
        Db.update (Upgrade.upgrade ~install_new:true conf packages);
        ignore (msgbox
          ~title:"Update successful"
          ~buttons:[ Button.ok ]
          (Lib.sp "The following packages have been updated successfully: %s"
            summary)
        )
      )
      else
        ()
