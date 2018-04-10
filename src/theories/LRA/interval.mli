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

type bound = Interval_sig.bound = Strict | Large

val pp_bound: bound Pp.pp

val compare_bounds_inf: Q.t * bound -> Q.t * bound -> int
val compare_bounds_sup: Q.t * bound -> Q.t * bound -> int
val compare_bounds_inf_sup: Q.t * bound -> Q.t * bound -> int

module Convexe: Interval_sig.S

module ConvexeWithExceptions: Interval_sig.S

module Union : Interval_sig.S

module ConvexeInfo(Info: sig
    include Stdlib.Datatype
    val nothing: t
  end) : sig
  include Stdlib.Datatype

  val invariant: t -> bool

  val is_distinct: t -> t -> bool
  val is_included: t -> t -> bool

  val mult_cst: Q.t -> t -> t
  val add_cst : Q.t -> t -> t
  val add: min_info:Info.t -> ?max_info:Info.t -> t -> t -> t
  val minus: min_info:Info.t -> ?max_info:Info.t -> t -> t -> t

  val mem: Q.t -> t -> bool

  (** from Q.t *)
  val singleton: min_info:Info.t -> ?max_info:Info.t -> Q.t -> t
  val is_singleton: t -> Q.t option

  val except: t -> Q.t -> t option

  val gt: min_info:Info.t -> Q.t -> t
  val ge: min_info:Info.t -> Q.t -> t
  val lt: max_info:Info.t -> Q.t -> t
  val le: max_info:Info.t -> Q.t -> t
  (** > q, >= q, < q, <= q *)

  val union: t -> t -> t
  (** union set *)

  val inter: t -> t -> t option
  (** intersection set.
      if the two arguments are equals, return the second
  *)


  val zero: ?max_info:Info.t -> min_info:Info.t -> t
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
