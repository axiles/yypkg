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

(** This module exists only in order to run the main by default only in
 * command-line mode (as opposed to GUI). *)

let () =
  Printexc.record_backtrace true;
  let b = Buffer.create 1000 in
  Yypkg_top.main_wrap b;
  Buffer.output_buffer stderr b;
  if Buffer.length b <> 0 then
    exit 1
