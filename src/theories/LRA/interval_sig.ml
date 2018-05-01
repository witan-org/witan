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

open Witan_popop_lib

type bound = Strict | Large

module type S = sig
  include Stdlib.Datatype

  val invariant: t -> bool

  val is_distinct: t -> t -> bool
  val is_included: t -> t -> bool

  val mult_cst: Q.t -> t -> t
  val add_cst : Q.t -> t -> t
  val add: t -> t -> t
  val minus: t -> t -> t

  val mem: Q.t -> t -> bool

  (** from Q.t *)
  val singleton: Q.t -> t
  val is_singleton: t -> Q.t option

  val except: t -> Q.t -> t option

  val gt: Q.t -> t
  val ge: Q.t -> t
  val lt: Q.t -> t
  val le: Q.t -> t
  (** > q, >= q, < q, <= q *)

  val union: t -> t -> t
  (** union set *)

  val inter: t -> t -> t option
  (** intersection set.
      if the two arguments are equals, return the second
  *)


  val zero: t
  val reals: t
  (** R *)
  val is_reals: t -> bool

  val choose: t -> Q.t
  (** Nothing smart in this choosing *)


  val choose_rnd : (int -> int) -> t -> Q.t
  (** choose an element randomly (but non-uniformly), the given function is
        the random generator *)

  val get_convexe_hull: t -> (Q.t * bound) option * (Q.t * bound) option

end
