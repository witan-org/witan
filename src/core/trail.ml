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

open Stdlib
open Typedef

let debugage = Debug.register_info_flag
    ~desc:"for@ the@ age in the trail."
    "Trail.age"

let debug = Debug.register_flag (** not info just because duplicate of solver *)
  ~desc:"for@ the@ trail."
  "Trail.core"

module Exp = Keys.Make_key(struct end)
module Cho = Keys.Make_key2(struct end)

type chogen =
  | GCho: Node.t * ('k,'d) Cho.t * 'k -> chogen

module Age = struct
  include DIntOrd
  let bef = -1
  let min = 0
  let max (x : int) y = if x < y then y else x
  let pred x = x - 1
  let succ x = x + 1
  let to_int x = x
end
type age = Age.t (* position in the trail *)

module Tag = Keys.Make_key(struct end)
type 'a tag = 'a Tag.t

module Tags : sig
  type t
  val empty: t
  val add: t -> 'a tag -> 'a Bag.t -> t
  val find: t -> 'a tag -> 'a Bag.t
  val union: t -> t -> t
  val pp: t Pp.pp
end = struct
  type exi
  type t = exi Bag.t Tag.K.M.t
  let empty = Tag.K.M.empty
  let add : type a. t -> a tag -> a Bag.t -> t =
    fun tags tag l ->
      Tag.K.M.add ((tag : a tag) :> Tag.K.t)
        (Obj.magic (l : a Bag.t) :> exi Bag.t) tags
  let find : type a. t -> a tag -> a Bag.t =
    fun tags tag ->
      (Obj.magic (Tag.K.M.find_def Bag.empty ((tag : a tag) :> Tag.K.t)
                    tags : exi Bag.t) : a Bag.t)
  let union : t -> t -> t = fun t1 t2 ->
         Tag.K.M.union (fun _ a b -> Some (Bag.concat a b)) t1 t2
  let pp fmt _ = Format.pp_print_string fmt "!Tags!"
end
type tags = Tags.t

type dec = age
let age_of_dec x = x
let print_dec = Age.pp

type pexp =
| Pexp: age * 'a Exp.t * 'a * tags -> pexp

(** Indicate when a node stopped to be the representative, a what it becomes.
    Can be used to know the state of the classes at any point in the past.
*)
type nodehist = (age * Node.t) Node.M.t

type t = {
  mutable last_dec : Age.t;
  mutable first_dec : Age.t;
  mutable nbdec    : int;
  mutable age      : Age.t;
  trail            : pexp Simple_vector.t;
  mutable nodehist : nodehist;
}

let create () = {
  last_dec = Age.bef;
  first_dec = max_int;
  nbdec = 0;
  age = Age.bef;
  trail = Simple_vector.create 10;
  nodehist = Node.M.empty;
}

let new_handle t = {
  last_dec = t.last_dec;
  first_dec = t.first_dec;
  nbdec = t.nbdec;
  age = t.age;
  trail = t.trail; (* not copied because we just add at the front new things *)
  nodehist = t.nodehist;
}

let new_dec (t:t)  =
  t.nbdec <- t.nbdec + 1;
  let dec = t.age + 1 in
  t.last_dec <- dec;
  if t.first_dec == max_int then t.first_dec <- dec;
  Debug.dprintf2 debug "[Trail] @[new dec %a@]" Age.pp dec;
  dec

let current_age t = t.age
let nbdec t = t.nbdec

let mk_pexp:
  t ->
  ?age:age (* in which age it should be evaluated *) ->
  ?tags:tags ->
  'a Exp.t -> 'a -> pexp =
  fun t ?(age=t.age) ?(tags=Tags.empty) exp e ->
    Pexp(age,exp,e,tags)

let add_pexp t pexp =
  t.age <- Age.succ t.age;
  Simple_vector.push t.trail pexp

let add_merge_start:
  t -> pexp -> node1:Node.t -> node2:Node.t ->
  node1_repr:Node.t -> node2_repr:Node.t -> new_repr:Node.t -> unit
  =
  fun _t _pexp ~node1:_ ~node2:_ ~node1_repr:_ ~node2_repr:_ ~new_repr:_ ->
    ()

let add_merge_finish:
  t -> pexp -> node1:Node.t -> node2:Node.t ->
  node1_repr:Node.t -> node2_repr:Node.t -> new_repr:Node.t -> unit
  =
  fun t pexp ~node1:_ ~node2:_ ~node1_repr ~node2_repr ~new_repr ->
    add_pexp t pexp;
    let old_repr = if Node.equal node1_repr new_repr then node2_repr else node1_repr in
    t.nodehist <- Node.M.add old_repr (t.age,new_repr) t.nodehist

let add_pexp_dom:
  t -> pexp -> 'b Dom.t -> node:Node.t -> node0:Node.t -> unit =
  fun _t _pexp _dom ~node:_ ~node0:_ ->
    assert false (** TODO when domain will be needed *)

let add_pexp_dom_premerge:
  t -> 'b Dom.t ->
  nodeto:Node.t ->
  nodefrom:Node.t ->
  nodefrom0:Node.t ->
  unit =
  fun _t _dom ~nodeto:_ ~nodefrom:_ ~nodefrom0:_ ->
    assert false (** TODO when domain will be needed *)


let expfact : unit Exp.t = Exp.create_key "Trail.fact"
let pexpfact = Pexp(Age.bef,expfact,(),Tags.empty)

type exp_same_sem =
| ExpSameSem   : pexp * Node.t * ThTerm.t -> exp_same_sem
| ExpSameValue : pexp * Node.t * Values.t -> exp_same_sem

let exp_same_sem : exp_same_sem Exp.t =
  Exp.create_key "Egraph.exp_same_sem"

(** TODO choose an appropriate data *)
let exp_diff_value : pexp Exp.t =
  Exp.create_key "Egraph.exp_diff_value"
