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

let read path = 
  let ic = open_in path in
  let sexp = try Pre_sexp.input_sexp ic with e -> close_in ic; raise e in
  close_in ic;
  sexp

let write path data =
  let flags = [ Open_creat; Open_binary; Open_wronly; Open_trunc ] in
  let oc = open_out_gen flags 0o644 path in
  (try output_string oc (Pre_sexp.to_string data) with e -> close_out oc; raise e);
  close_out oc

