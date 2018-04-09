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

val cst : Q.t -> Node.t
val add' : Q.t -> Node.t -> Q.t -> Node.t -> Node.t
val add : Node.t -> Node.t -> Node.t
val sub : Node.t -> Node.t -> Node.t

val mult_cst : Q.t -> Node.t -> Node.t


val mult : Node.t -> Node.t -> Node.t

val th_register : Egraph.d -> unit
val zero: Node.t

val gt_zero: Node.t -> Node.t
val ge_zero: Node.t -> Node.t
val lt: Node.t -> Node.t -> Node.t
val le: Node.t -> Node.t -> Node.t
val gt: Node.t -> Node.t -> Node.t
val ge: Node.t -> Node.t -> Node.t
