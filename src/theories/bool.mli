(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2013                                                    *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Witan_core

type t
val sem: t Sem.t
val dom: bool Dom.t

val _true : Cl.t
val _false : Cl.t
val _and  : Cl.t list -> Cl.t
val _or   : Cl.t list -> Cl.t
val _not  : Cl.t -> Cl.t
val gen   : bool -> (Cl.t * bool) list -> Cl.t
(** [gen d b0 [cl1,b1;cl2,c2]] is
    not_b0 (or (not_b1 cl1,not_b2 cl2)) with not_x f = if x then not f else f
*)

val set_true  : Solver.d -> Explanation.pexp -> Cl.t -> unit
val set_false : Solver.d -> Explanation.pexp -> Cl.t -> unit

val is       : Solver.d -> Cl.t -> bool option
val is_true  : Solver.d -> Cl.t -> bool
val is_false : Solver.d -> Cl.t -> bool
(** is_true t cl = false means the value is not constrained by the
    current constraints or due to incompletness *)
val is_unknown : Solver.d -> Cl.t -> bool

val true_is_false : Solver.d -> Cl.t -> Explanation.pexp -> 'a

val th_register: Solver.d -> unit
val th_register_alone: Solver.d -> unit

val chobool: (Cl.t,bool) Explanation.cho

val make_dec: Variable.make_dec

val ty: Ty.t
val ty_ctr: Ty.Constr.t
