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

open Stdlib

module Constr= Strings.Fresh(struct end)

type ty = { ctr: Constr.t; args: ty IArray.t; tag: int}

module Ty = Hashcons.Make(struct
    type t = ty

    let equal ty1 ty2 =
      Constr.equal ty1.ctr ty2.ctr &&
      IArray.equal (fun x1 x2 -> DInt.equal x1.tag x2.tag) ty1.args ty2.args

    let hash {ctr;args} =
      Hashcons.combine (Constr.hash ctr)
        (IArray.hash (fun x1 -> x1.tag) args)

    let set_tag i t = {t with tag = i}
    let tag t = t.tag

    let rec pp fmt = function
      | {ctr;args} when IArray.length args = 0 -> Constr.pp fmt ctr
      | {ctr;args} -> Format.fprintf fmt "%a(%a)"
                        Constr.pp ctr (IArray.pp Pp.comma pp) args
  end)

let app ctr args = Ty.hashcons {ctr;args; tag = -1}
let args0 = IArray.of_array [||]
let ctr ctr = app ctr args0

include Ty
