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

(** Keep track of the explanation, conflict and choice *)

open Witan_popop_lib
open Nodes

(** {2 Age: position in the trail } *)

(** The age is a position in the trail. So it is also a way to
    indicate the state. The state corresponding to an age is the state
    of the solver when the current age integer have been incremented
    into this age *)

module Age: sig
  include Stdlib.Datatype
  val min: t
  val max: t -> t -> t
  val pred: t -> t
  val succ: t -> t
  val to_int: t -> int
  val of_int: int -> t

  val (<) : t -> t -> bool
  val (<=): t -> t -> bool
  val (>) : t -> t -> bool
  val (>=): t -> t -> bool
end
type age = Age.t

(** {2 Trail, Conflict and Choices } *)

module Exp: Keys.Key

module Pexp : sig
  type t =
    | Pexp: age * 'a Exp.t * 'a -> t
    (** An explanation. The age indicate the state to consider *)

  val pp: t Format.printer

end

module Hyp: Keys.Key

module Phyp : sig
  type t =
    | Phyp: 'a Hyp.t * 'a * [`Dec | `NoDec]-> t

  val phyp: ?dec:unit -> 'a Hyp.t -> 'a -> t

  val map: 'a Hyp.t -> 'a list -> t list

end

(** trail and additional information *)
type t
val create: Context.creator -> t

val current_age: t -> age
val print_current_age: t Format.printer
val last_dec: t -> age
val before_last_dec: t -> age -> bool
val before_first_dec: t -> age -> bool

val get_pexp: t -> age -> Pexp.t

(** {2 Decisions} *)
type dec

val print_dec: dec Format.printer
val age_of_dec: dec -> age

(** mark a new decisions *)
val new_dec: t -> dec
val nbdec: t -> int

(** {2 Trails} *)
val mk_pexp:
  t ->
  ?age:age (* in which age it should be evaluated *) ->
  'a Exp.t -> 'a -> Pexp.t
(** create a new explanation using by default the current age *)

val add_merge_start:
  t -> Pexp.t ->
  node1:Node.t -> node2:Node.t ->
  node1_repr:Node.t -> node2_repr:Node.t -> new_repr:Node.t -> unit
(** Start of merge, indicative mainly for domains *)

val add_merge_finish:
  t -> Pexp.t ->
  node1:Node.t -> node2:Node.t ->
  node1_repr:Node.t -> node2_repr:Node.t -> new_repr:Node.t -> unit
(** End of a merge, pexp is added to the trail  *)


(** {2 Predefined explanation} *)

val exp_fact: unit Exp.t
val pexp_fact: Pexp.t
(** No need of any explanation it is a fact. Perhaps should be avoided
    for proof generation *)

type exp_same_sem =
| ExpSameSem   : Pexp.t * Node.t * ThTerm.t -> exp_same_sem
| ExpSameValue : Pexp.t * Node.t * Value.t -> exp_same_sem

val exp_same_sem : exp_same_sem Exp.t
(** Two nodes have been merged because they shared the same semantical
    terms or value *)

val exp_diff_value: (Value.t * Node.t * Node.t * Value.t * Pexp.t) Exp.t
(** A contradiction have been reached because the given explanation
    makes one equivalence class be associated to two different values *)

(** {2 Generic choices} *)

module Cho: Keys.Key

(** Generic decision *)
type chogen =
  | GCho: Node.t * 'k Cho.t * 'k -> chogen

val add_pexp: t -> Pexp.t -> unit
(** generic *)

(** {2 Get information from trail for conflict} *)

val age_merge_opt: t -> Node.t -> Node.t -> Age.t option
(** Give the age at which the given node merged *)

(** backward reference *)

val _pp_pexp: Pexp.t Format.printer ref
