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

open Witan_core_structures

module Cho = Trail.Cho

type 'd decdone  =
| DecNo
| DecTodo of 'd

module type Cho = sig
  module OnWhat  : sig
    type t
    val pp: t Pp.pp
  end

  module What: sig
    type t
    val pp: t Pp.pp
  end

  val choose_decision:
    Egraph.Delayed.t -> OnWhat.t -> What.t decdone

  val make_decision:
    Egraph.Delayed.t -> Trail.dec -> OnWhat.t -> What.t -> unit

  val key: (OnWhat.t,What.t) Cho.t

end


module Conflict = struct
  type t
end

module Exp = Trail.Exp
module Con = Keys.Make_key(struct end)

type congen =
  | GCon: 'a Con.t * 'a -> congen

module type Exp = sig

  type t

  val pp: t Pp.pp

  val key: t Trail.Exp.t

  val analyse  :
    Conflict.t -> Trail.Age.t -> t -> 'a Con.t -> 'a -> congen
end

let register_exp: (module Exp) -> unit = fun _ -> assert false

type levels = {levels: 'a. Typedef.Node.t -> 'a Typedef.Value.t -> unit}

module type Con = sig

  type t

  val pp: t Pp.pp

  val apply_learnt: t -> Typedef.Node.t

  val levels: levels -> t -> unit

end

let register_con: (module Con) -> unit = fun _ -> assert false


let contradiction = Con.create_key "contradiction"


let () = register_con (module (struct
                        include Stdlib.DUnit
                        let apply_learnt () = raise Std.Impossible
                        let levels _ () = raise Std.Impossible
                      end))

let is_con_contradiction con = Con.equal contradiction con
