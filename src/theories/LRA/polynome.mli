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
(** Polynome *)

type t = private { cst : Q.t; poly : Q.t Node.M.t}
include Stdlib.Datatype with type t := t

val invariant: t -> bool

val zero: t
val is_zero: t -> bool

val cst: Q.t -> t
val is_cst: t -> Q.t option
val monome: Q.t -> Node.t  -> t
val is_one_node: t -> Node.t option

type extract =
  | Zero            (** p = 0 *)
  | Cst of Q.t      (** p = q *)
  | Var of Q.t * Node.t * t (** p = qx + p' *)

val extract : t -> extract

type kind = | ZERO | CST | VAR
val classify : t -> kind

val sub_cst: t -> Q.t -> t
val mult_cst: Q.t -> t -> t

val add: t -> t -> t
val sub: t -> t -> t

val of_list: Q.t -> (Node.t * Q.t) list -> t

val x_p_cy: t -> Q.t -> t -> t

val cx_p_cy: Q.t -> t -> Q.t -> t -> t

val subst: t -> Node.t -> t -> t * Q.t
val subst_node: t -> Node.t -> Node.t -> t * Q.t

val fold: ('a -> Node.t -> Q.t -> 'a) -> 'a -> t -> 'a
val iter: (Node.t -> Q.t -> unit) -> t -> unit

type 'a tree =
  | Empty
  | Node of 'a tree * Node.t * 'a * 'a tree * int

val get_tree: t -> Q.t tree * Q.t
