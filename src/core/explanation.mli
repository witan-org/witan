(*************************************************************************)
(*  This file is part of Witan.                                          *)
(*                                                                       *)
(*  Copyright (C) 2017                                                   *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies           *)
(*           alternatives)                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en        *)
(*           Automatique)                                                *)
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

(** {2 Explanation, Conflict and Choices } *)

module Exp: Keys.Key
module Con: Keys.Key
module Cho: Keys.Key2

type 'a exp = 'a Exp.t
type 'a con = 'a Con.t
type ('k,'d) cho = ('k,'d) Cho.t

type pexp =
| Pexp: age * 'a exp * 'a * tags -> pexp
(** An explanation. The age indicate the state to consider *)


type t
val create: unit -> t
val new_handle: t -> t

val current_age: t -> age

type dec

val print_dec: dec Pp.pp
val age_of_dec: dec -> age

val new_dec: t -> dec
val nbdec: t -> int

val mk_pexp:
  t ->
  ?age:age (* in which age it should be evaluated *) ->
  ?tags:tags ->
  'a exp -> 'a -> pexp
(** create a new explanation using by default the current age *)

val add_pexp_equal:
  t -> pexp ->
  node1:Node.t -> node2:Node.t ->
  node1_repr:Node.t -> node2_repr:Node.t -> new_repr:Node.t -> unit
(** Add in the trail the explanation for the equality between node1
    and node2 (the order is important) *)

val add_pexp_value:
  t -> pexp -> 'b value -> node:Node.t -> node_repr:Node.t -> unit
(** Add in the trail the explanation for the setting of a value to a node *)

type chogen =
  | GCho: ('k,'d) cho * 'k -> chogen

val expfact: unit exp
val pexpfact: pexp



(** {2 Explanation for domains, currently not used } *)

val add_merge_dom_no:
  t -> inv:bool -> other_node:Node.t -> other_node0:Node.t
  -> repr_node:Node.t -> repr_node0:Node.t -> unit
(** End of a merge without any domain merge needed *)

val add_merge_dom_all:
  t -> inv:bool -> other_node:Node.t -> other_node0:Node.t
  -> repr_node:Node.t -> repr_node0:Node.t -> unit
(** End of a merge with some merge done *)

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
