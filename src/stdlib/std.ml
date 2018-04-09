(*************************************************************************)
(*  This file is part of Witan.                                          *)
(*                                                                       *)
(*  Copyright (C) 2017                                                   *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies           *)
(*           alternatives)                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en        *)
(*           Automatique)                                                *)
(*    CNRS  (Centre national de la recherche scientifique)               *)
(*                                                                       *)
(*  you can redistribute it and/or modify it under the terms of the GNU  *)
(*  Lesser General Public License as published by the Free Software      *)
(*  Foundation, version 2.1.                                             *)
(*                                                                       *)
(*  It is distributed in the hope that it will be useful,                *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(*  GNU Lesser General Public License for more details.                  *)
(*                                                                       *)
(*  See the GNU Lesser General Public License version 2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).           *)
(*************************************************************************)

exception Impossible (* Absurd *)
exception TODO of string

module Q = struct
  module Q = struct
    include Q
    let hash = Hashtbl.hash
    let pp fmt q =
      match Q.classify q with
      | Q.ZERO  -> Pp.char   fmt '0'
      | Q.INF   -> Pp.string fmt "+∞"
      | Q.MINF  -> Pp.string fmt "-∞"
      | Q.UNDEF -> Pp.string fmt "!undef!"
      | Q.NZERO -> Q.pp_print fmt q
  end
  include Q
  let le = leq
  let ge = geq
  let two = Q.of_int 2
  include Stdlib.MkDatatype(Q)

  let of_string_decimal =
    let decimal = Str.regexp "\\(+\\|-\\)?\\([0-9]+\\)\\([.]\\([0-9]*\\)\\)?" in
    fun s ->
      if not (Str.string_match decimal s 0) then None
      else
        let sgn = match Str.matched_group 1 s with
          | "-" -> Q.minus_one
          | "+" -> Q.one
          | exception Not_found -> Q.one
          | _ -> assert false in
        let int_part = Q.of_string (Str.matched_group 2 s) in
        let dec_part = match Str.matched_group 4 s with
          | exception Not_found -> Q.zero
          | "" -> Q.zero
          | dec ->
            let l = String.length dec in
            let dec = Q.of_string dec in
            let ten = Q.of_int 10 in
            Witan_popop_lib.Util.foldi (fun acc _ -> Q.(acc * ten)) dec 1 l
        in
        Some Q.(sgn * (int_part + dec_part))
end
