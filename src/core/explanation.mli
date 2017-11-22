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

open Typedef


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



module Age: sig
  include Stdlib.Datatype
  val min: t
  val max: t -> t -> t
  val pred: t -> t
  val succ: t -> t
  val to_int: t -> int
end
type age = Age.t



module Exp: Keys.Key
module Con: Keys.Key
module Cho: Keys.Key2

type 'a exp = 'a Exp.t
type 'a con = 'a Con.t
type ('k,'d) cho = ('k,'d) Cho.t

type 'a rescon =
| GRequested: 'a -> 'a rescon
| GOther: 'b con * 'b ->  'a rescon

type concache
type pexp = private
  | Pexp: age * 'a exp * 'a * tags * concache -> pexp
  [@@deriving show]

type dec

type modif =
| Node : Node.t * Node.t               -> modif
  (** Node(cl1,cl2,pexp) explication why cl1 and cl2 are merged
      but cl1 and cl2 are perhaps not the representative of there class
  *)

| Dom: Node.t * 'a Dom.t      * pexp * Node.t -> modif
(** Node(clr,dom,pexp,node) explication why cl1 and cl2 are merged
      but clr is the representative of the equivalence class,
    cl2 can be not the representative.
*)

| DomL: Node.t * 'a Dom.t * 'a option * Age.t * pexp * Node.t -> modif
(** same as before but the first time in this level *)

| Dec: dec                       -> modif
(** new level decision *)

type node_clhist = {
  nage : age;
  ncl : Node.t;
  npexp: pexp;
  ninv : bool;
}

val print_node: node_clhist Pp.pp

type clgraph = (node_clhist list) Node.M.t (** graph *)
type clhist = (age * Node.t) Node.M.t (** graph *)

type mod_dom = {
  modcl : Node.t;
  modage : Age.t;
  modpexp : pexp
}

val print_mod_dom: mod_dom Pp.pp

type domhist_node =
  | DomNeverSet
  | DomMerge of Age.t (** agedommerge *) *
                domhist_node (** other_cl *) *
                domhist_node (** repr_cl  *)
  | DomPreMerge of Age.t *
                   Node.t * (** node that will be equal to it
                              and from which we take the node *)
                   domhist_node * (** domhist of this node *)
                   domhist_node   (** previous domhist *)
  | DomSet of mod_dom * domhist_node

val print_domhist_node: domhist_node Pp.pp

type domhist = domhist_node Node.M.t Dom.Vector.t

val print_domhist: domhist Pp.pp

type dom_before_last_dec =
  { dom_before_last_dec: 'a. 'a Dom.t -> Node.t -> 'a option }

type t = private {
  mutable last_dec : Age.t;
  mutable first_dec : Age.t;
  mutable nbdec    : int;
  mutable age      : Age.t;
  mutable trail    : modif list;
  mutable clgraph   : clgraph;
  mutable clhist   : clhist;
  mutable dom_before_last_dec: dom_before_last_dec;
  domhist      : domhist;
}

val create: unit -> t
val new_handler: t -> t

val current_age: t -> age

val mk_pcho: dec -> ('k,'d) cho -> 'k -> 'd -> pexp

val print_dec: dec Pp.pp
val age_of_dec: dec -> age
val new_dec: dom_before_last_dec -> t -> dec


val mk_pexp:
  t ->
  ?age:age (* in which age it should be evaluated *) ->
  ?tags:tags ->
  'a exp -> 'a -> pexp


(** TODO make the add_pexp_cl sooner
    and so cut in two it.
 *)
val add_pexp_cl:
  t -> pexp -> inv:bool -> other_cl:Node.t -> other_cl0:Node.t
  -> repr_cl:Node.t -> repr_cl0:Node.t -> unit
(** node* representative, node*_0 the one merged initially on which the
    pexp apply *)
val add_merge_dom_no:
  t -> inv:bool -> other_cl:Node.t -> other_cl0:Node.t
  -> repr_cl:Node.t -> repr_cl0:Node.t -> unit

val add_merge_dom_all:
  t -> inv:bool -> other_cl:Node.t -> other_cl0:Node.t
  -> repr_cl:Node.t -> repr_cl0:Node.t -> unit
(** node* representative, node*_0 the one merged initially on which the
    pexp apply *)
val add_pexp_dom:
  t -> pexp -> 'b Dom.t -> node:Node.t -> cl0:Node.t -> unit
val add_pexp_value:
  t -> pexp -> 'b value -> node:Node.t -> cl0:Node.t -> unit


val add_pexp_dom_premerge:
  t -> 'b Dom.t ->
  clto:Node.t ->
  clfrom:Node.t ->
  clfrom0:Node.t ->
  unit

val trail: t -> modif list
val last_dec: t -> Age.t
val dom_before_last_dec: t -> 'a Dom.t -> Node.t -> 'a option
val nbdec: t -> int
val at_current_level: t -> Age.t -> bool
val before_first_dec : t -> Age.t -> bool
val pexpfact: pexp

(** Just for Conflict *)
module Conunknown : Intmap_hetero.S1 with
                    type 'a key = 'a con and type ('a,'b) data = 'a Bag.t
type conunknown = unit Conunknown.t
type chogen =
  | GCho: ('k,'d) cho * 'k -> chogen
type decs = chogen list Node.M.t Dom.M.t

val print_modif_ref : modif Pp.pp ref

type invclhist = Node.t Age.M.t Node.H.t
val print_invclhist: invclhist Pp.pp
val invclhist: t -> invclhist

(** Module for manipulating explicit dependencies *)
module Deps : sig
  type t

  val empty: t
  val is_empty: t -> bool
  val concat: t -> t -> t
  val concatl: t list -> t

  val add_tags: t -> tags -> t
  val add_unknown_con: t -> 'a con -> 'a -> t
  val add_chogen: t -> chogen -> t
end


module Deps_Result: sig
  type t = {
    unknown : conunknown;
    tags    : tags;
    decs     : chogen Bag.t;
  }

  val empty: t
  val concat: t -> t -> t

  val compute_deps: Deps.t -> t
  (** Can be done only by Conflict and only once!!! *)
end

module Concache : sig
  type 'a value = 'a rescon * Deps.t
  val set    : concache -> 'a con -> 'a value -> unit
  val get    : concache -> 'a con -> 'a value
  val is_set : concache -> 'a con -> bool
  val clear  : concache -> unit
end


val mk_pexp_direct:
  age:age (* in which age it should be evaluated *) ->
  ?tags:tags ->
  'a exp -> 'a -> pexp

type pcho =
  | Pcho: dec * ('k,'d) cho * 'k * 'd -> pcho

val expcho: pcho exp
val expfact: unit exp

val pp_pexp_ref : (pexp Pp.pp) ref
