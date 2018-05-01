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

open Witan_popop_lib
open Stdlib
open Nodes

let debugage = Debug.register_info_flag
    ~desc:"for@ the@ age in the trail."
    "Trail.age"

let debug = Debug.register_flag (** not info just because duplicate of solver *)
  ~desc:"for@ the@ trail."
  "Trail.core"

module Exp = Keys.Make_key(struct end)
module Cho = Keys.Make_key(struct end)

type chogen =
  | GCho: Node.t * 'k Cho.t * 'k -> chogen

module Age = struct
  include DIntOrd
  let bef = -1
  let min = 0
  let max (x : int) y = if x < y then y else x
  let pred x = x - 1
  let succ x = x + 1
  let to_int x = x
  let of_int x = x

  let (<)  : t -> t -> bool = (<)
  let (<=) : t -> t -> bool = (<=)
  let (>)  : t -> t -> bool = (>)
  let (>=) : t -> t -> bool = (>=)

end
type age = Age.t (* position in the trail *)

type dec = age
let age_of_dec x = x
let print_dec = Age.pp

module Pexp = struct
  type t =
    | Pexp: age * 'a Exp.t * 'a -> t

  let _pp_pexp : t Pp.pp ref = ref (fun _ _ -> assert false)

  let pp fmt x = !_pp_pexp fmt x
end

let _pp_pexp = Pexp._pp_pexp

module Hyp = Keys.Make_key(struct end)

module Phyp = struct
  type t =
    | Phyp: 'a Hyp.t * 'a * [`Dec | `NoDec]-> t

  (** `Dec indiquate the conflict come from the explanation of a
     decision and then should not be explained further *)
  let phyp ?(dec:unit option) c v = Phyp(c,v,
                                         if Equal.option Equal.unit dec None
                                         then `NoDec else `Dec)
  let map c l = List.map (phyp c) l
end

(** Indicate when a node stopped to be the representative, and what it becomes.
    Can be used to know the state of the classes at any point in the past.
*)
type nodehist = (age * Node.t) Node.M.t

type t = {
  mutable last_dec : Age.t;
  mutable first_dec : Age.t;
  mutable nbdec    : int;
  mutable age      : Age.t;
  trail            : Pexp.t Simple_vector.t;
  mutable nodehist : nodehist;
}

let before_last_dec t a = Age.compare a t.last_dec < 0
let before_first_dec t a = Age.(a < t.first_dec)

let get_pexp t age = Simple_vector.get t.trail age

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
  if Equal.physical t.first_dec max_int then t.first_dec <- dec;
  Debug.dprintf2 debug "[Trail] @[new dec %a@]" Age.pp dec;
  dec

let current_age t = t.age
let print_current_age fmt t = Age.pp fmt t.age
let last_dec t = t.last_dec
let nbdec t = t.nbdec

let mk_pexp:
  t ->
  ?age:age (* in which age it should be evaluated *) ->
  'a Exp.t -> 'a -> Pexp.t =
  fun t ?(age=t.age) exp e ->
    Pexp(age,exp,e)

let add_pexp t pexp =
  t.age <- Age.succ t.age;
  Simple_vector.inc_size (t.age + 1) t.trail;
  Simple_vector.set t.trail t.age pexp

let add_merge_start:
  t -> Pexp.t -> node1:Node.t -> node2:Node.t ->
  node1_repr:Node.t -> node2_repr:Node.t -> new_repr:Node.t -> unit
  =
  fun _t _pexp ~node1:_ ~node2:_ ~node1_repr:_ ~node2_repr:_ ~new_repr:_ ->
    ()

let add_merge_finish:
  t -> Pexp.t -> node1:Node.t -> node2:Node.t ->
  node1_repr:Node.t -> node2_repr:Node.t -> new_repr:Node.t -> unit
  =
  fun t pexp ~node1:_ ~node2:_ ~node1_repr ~node2_repr ~new_repr ->
    add_pexp t pexp;
    let old_repr = if Node.equal node1_repr new_repr then node2_repr else node1_repr in
    t.nodehist <- Node.M.add old_repr (t.age,new_repr) t.nodehist

let exp_fact = Exp.create_key (module struct type t = unit let name = "Trail.fact" end)
let pexp_fact = Pexp.Pexp(Age.bef,exp_fact,())

type exp_same_sem =
| ExpSameSem   : Pexp.t * Node.t * ThTerm.t -> exp_same_sem
| ExpSameValue : Pexp.t * Node.t * Value.t -> exp_same_sem

let exp_same_sem =
  Exp.create_key (module struct
    type t = exp_same_sem
    let name = "Egraph.exp_same_sem"
  end)

(** TODO choose an appropriate data *)
let exp_diff_value =
  Exp.create_key (module struct
    type t = Value.t * Node.t * Node.t * Value.t * Pexp.t
    let name = "Egraph.exp_diff_value"
  end)


let age_merge_opt t n1 n2 =
  if Node.equal n1 n2 then Some Age.min
  else
    let rec aux t n1 n2 =
      assert (not (Node.equal n1 n2));
      let ret age n1 n2 = if Node.equal n1 n2 then Some age else aux t n1 n2 in
      match n1, Node.M.find_opt n1 t.nodehist, n2, Node.M.find_opt n2 t.nodehist with
      | _, None, _, None -> None
      | n1, None, _, Some(age,n2) | _, Some(age,n2), n1, None ->
        ret age n1 n2
      | n1, Some(age1,n1'), n2, Some(age2,n2') ->
        let c = Age.compare age1 age2 in
        assert ( c <> 0);
        if c < 0 then ret age1 n1' n2 else ret age2 n1 n2'
    in
    aux t n1 n2
