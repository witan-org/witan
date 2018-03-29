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

open Witan_core_structures

type t
val sem: t Sem.t
val dom: bool Value.t

val _true : Node.t
val _false : Node.t
val _and  : Node.t list -> Node.t
val _or   : Node.t list -> Node.t
val _not  : Node.t -> Node.t
val gen   : bool -> (Node.t * bool) list -> Node.t
(** [gen d b0 [cl1,b1;cl2,c2]] is
    not_b0 (or (not_b1 cl1,not_b2 cl2)) with not_x f = if x then not f else f
*)

val set_true  : Egraph.d -> Trail.Pexp.t -> Node.t -> unit
val set_false : Egraph.d -> Trail.Pexp.t -> Node.t -> unit

val is       : Egraph.d -> Node.t -> bool option
val is_true  : Egraph.d -> Node.t -> bool
val is_false : Egraph.d -> Node.t -> bool
(** is_true t node = false means the value is not constrained by the
    current constraints or due to incompletness *)
val is_unknown : Egraph.d -> Node.t -> bool

(* val true_is_false : Egraph.d -> Node.t -> Trail.Pexp.t -> 'a *)

val th_register: Egraph.d -> unit

val chobool: Node.t Trail.Cho.t

(* val make_dec: Variable.make_dec *)

val ty: Ty.t

module BoolValue : Typedef.RegisteredValue with type s = bool

val value_true : BoolValue.t
val value_false: BoolValue.t
val values_true : Values.t
val values_false: Values.t
