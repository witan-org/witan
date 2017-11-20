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

open Typedef

type make_dec = Cl.t -> Explanation.chogen

val cst: Ty.t -> string -> Cl.t
(** same string, same class *)

val fresh: Ty.t -> string -> Cl.t
(** always fresh *)

val add_dec: dec:make_dec -> Solver.Delayed.t -> Cl.t -> unit
(** Ask for a decision for this *)

val register_sort: dec:make_dec -> Ty.t -> unit

val th_register: Solver.Delayed.t -> unit
(** Run on every solver that will use these function *)
