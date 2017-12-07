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

open Typedef


(** {2 Allows to track information (ex:unsat core),
    but currently not used } *)
module Tag: Keys.Key
type 'a tag = 'a Tag.t

module Tags : sig
  type t
  val empty: t
  val add: t -> 'a tag -> 'a Bag.t -> t
  val find: t -> 'a tag -> 'a Bag.t
  val union: t -> t -> t
  val pp: t Pp.pp
end
type tags = Tags.t

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
end
type age = Age.t

(** {2 Trail, Conflict and Choices } *)

module Exp: Keys.Key

type pexp =
| Pexp: age * 'a Exp.t * 'a * tags -> pexp
(** An explanation. The age indicate the state to consider *)


(** trail and additional information *)
type t
val create: unit -> t
val new_handle: t -> t

val current_age: t -> age

(** {2 Decisions} *)
type dec

val print_dec: dec Pp.pp
val age_of_dec: dec -> age

(** mark a new decisions *)
val new_dec: t -> dec
val nbdec: t -> int

(** {2 Trails} *)
val mk_pexp:
  t ->
  ?age:age (* in which age it should be evaluated *) ->
  ?tags:tags ->
  'a Exp.t -> 'a -> pexp
(** create a new explanation using by default the current age *)

val add_merge_start:
  t -> pexp ->
  node1:Node.t -> node2:Node.t ->
  node1_repr:Node.t -> node2_repr:Node.t -> new_repr:Node.t -> unit
(** Start of merge, indicative mainly for domains *)

val add_merge_finish:
  t -> pexp ->
  node1:Node.t -> node2:Node.t ->
  node1_repr:Node.t -> node2_repr:Node.t -> new_repr:Node.t -> unit
(** End of a merge, pexp is added to the trail  *)


(** {2 Predefined explanation} *)

val expfact: unit Exp.t
val pexpfact: pexp
(** No need of any explanation it is a fact. Perhaps should be avoided
    for proof generation *)

type exp_same_sem =
| ExpSameSem   : pexp * Node.t * NodeSem.t -> exp_same_sem
| ExpSameValue : pexp * Node.t * NodeValue.t -> exp_same_sem

val exp_same_sem : exp_same_sem Exp.t
(** Two nodes have been merged because they shared the same semantical
    terms or value *)

val exp_diff_value: pexp Exp.t
(** A contradiction have been reached because the given explanation
    makes one equivalence class be associated to two different values *)

(** {2 Generic choices} *)

module Cho: Keys.Key2

(** Generic decision *)
type chogen =
  | GCho: Node.t * ('k,'d) Cho.t * 'k -> chogen


(** {2 Trail for domains, currently not used } *)

val add_pexp_dom:
  t -> pexp -> 'b Dom.t -> node:Node.t -> node0:Node.t -> unit
(** A domain has been modified *)

val add_pexp_dom_premerge:
  t -> 'b Dom.t ->
  nodeto:Node.t ->
  nodefrom:Node.t ->
  nodefrom0:Node.t ->
  unit
(** A domain has been modified during the merge of classes *)
