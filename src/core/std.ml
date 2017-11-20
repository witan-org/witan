(*************************************************************************)
(*  This file is part of Witan.                                          *)
(*                                                                       *)
(*  Copyright (C) 2017                                                   *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies           *)
(*           alternatives)                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en        *)
(*           Automatique)                                                *)
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
end
