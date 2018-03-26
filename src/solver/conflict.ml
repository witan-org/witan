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

open Witan_core_structures

module Levels = struct
  type t =
    | No
    | One of Trail.Age.t
    | Two of Trail.Age.t * Trail.Age.t
  [@@ deriving eq]

  let compare l1 l2 =
    match l1, l2 with
    | No, No -> 0
    | _, No -> 1
    | No, _ -> -1
    | One a, One b -> Trail.Age.compare a b
    | One a, Two (b,_) ->
      let c = Trail.Age.compare a b in
      if c <> 0 then c else -1
    | Two (a,_), One b ->
      let c = Trail.Age.compare a b in
      if c <> 0 then c else 1
    | Two (a1,b1), Two(a2,b2) ->
      let c = Trail.Age.compare a1 a2 in
      if c <> 0 then c else
      let c = Trail.Age.compare b1 b2 in
      c

  let invariant = function
    | No -> true
    | One _ -> true
    | Two (a,b) -> Trail.Age.compare a b >= 0

  let empty = No

  let add _ age = function
    | No -> One age
    | One age0 when Trail.Age.compare age0 age <= 0 -> Two(age,age0)
    | One age0 -> Two(age0,age)
    | Two(age0,_) when Trail.Age.compare age0 age <= 0 -> Two(age,age0)
    | Two(_,age1) as t when Trail.Age.compare age age1 <= 0 -> t
    | Two(age0,_)  -> Two(age0,age)

  let merge t1 = function
    | No -> t1
    | One age -> add () age t1
    | Two (age0,age1) -> add () age0 (add () age1 t1)


  let before_last_dec t = function
    | No -> true
    | One a -> Trail.before_last_dec t a
    | Two (a,_) -> Trail.before_last_dec t a

  let at_most_one_after_last_dec t = function
    | No -> true
    | One _ -> true
    | Two (_,b) -> Trail.before_last_dec t b

  let get_second_last = function
    | No | One _ -> Trail.Age.min
    | Two(_,b) -> b

  let get_last = function
    | No -> assert false
    | One a
    | Two(a,_) -> a

end

module Cho = Trail.Cho

type 'd decdone  =
| DecNo
| DecTodo of 'd

module type Cho = sig
  module OnWhat  : sig
    type t
    val pp: t Pp.pp
  end

  module What: sig
    type t
    val pp: t Pp.pp
  end

  val choose_decision:
    Egraph.Delayed.t -> OnWhat.t -> What.t decdone

  val make_decision:
    Egraph.Delayed.t -> Trail.Pexp.t -> OnWhat.t -> What.t -> unit

  val key: (OnWhat.t,What.t) Cho.t

end


module Conflict = struct
  type t = Trail.t

  let age_merge t n1 n2 = Trail.age_merge t n1 n2

end

module Exp = Trail.Exp
module Con = Trail.Con

module type Exp = sig

  type t

  val pp: t Pp.pp

  val key: t Trail.Exp.t


  val from_contradiction:
    Conflict.t (* -> Trail.Age.t *) -> t -> Trail.Pcon.t list
    (** First step of the analysis done on the trail. *)

  val analyse  :
    Conflict.t (* -> Trail.Age.t *) -> t -> 'a Con.t -> 'a -> Trail.Pcon.t list
end

module ExpRegistry = Exp.Make_Registry(struct
    type 'a data = (module Exp with type t = 'a)
    let pp (type a) (exp: a data) =
      let module Exp = (val exp) in
      Exp.pp
    let key (type a) (exp: a data) =
      let module Exp = (val exp) in
      Exp.key
  end)

let register_exp (exp:(module Exp)) =
  let module Exp = (val exp) in
  let exp = (module Exp : Exp with type t = Exp.t) in
  ExpRegistry.register exp

(* type levels = {levels: 'a. Typedef.Node.t -> 'a Typedef.Value.t -> unit} *)

module type Con = sig

  type t

  val pp: t Pp.pp

  val key: t Trail.Con.t

  val apply_learnt: t -> Typedef.Node.t

  val levels: Conflict.t -> t -> Levels.t

end

module ConRegistry = Con.Make_Registry(struct
    type 'a data = (module Con with type t = 'a)
    let pp (type a) (con: a data) =
      let module Con = (val con) in
      Con.pp
    let key (type a) (con: a data) =
      let module Con = (val con) in
      Con.key
  end)

let register_con (con:(module Con)) =
  let module Con = (val con) in
  let con = (module Con : Con with type t = Con.t) in
  ConRegistry.register con

let convert_lcon t l =
  let map (type a) con (c:a) pc =
    let module Con = (val (ConRegistry.get con) : Con with type t = a) in
    (Con.levels t c, pc)
  in
  List.map (fun (Trail.Pcon.PCon(con,c) as pc) -> map con c pc) l

let compare_level_con (l1,_) (l2,_) = - (Levels.compare l1 l2)

let sort_lcon l = List.sort compare_level_con l

let merge_lcon l1 l2 = List.merge compare_level_con l1 l2

let lcon_to_node l =
  let map (type a) con (c:a) =
    let module Con = (val (ConRegistry.get con) : Con with type t = a) in
    Con.apply_learnt c
  in
  List.map (fun (Trail.Pcon.PCon(con,c)) -> map con c) l

let _or = ref (fun _ -> assert false)

let learn trail (Trail.Pexp.Pexp(_,exp,x)) =
  let t = trail in
  let module Exp = (val (ExpRegistry.get exp)) in
  let l = Exp.from_contradiction trail x in
  let lcon = sort_lcon (convert_lcon t l) in
  let rec aux = function
    | [] -> assert false (** absurd: understand why *)
    | (Levels.No,_)::_                           -> assert false (** absurd: understand when *)
    | (l1,_)::_ when Levels.before_last_dec t l1 -> assert false (** absurd: understand when *)
    | (l1,_)::[] as l when Levels.at_most_one_after_last_dec t l1 ->
      (Levels.get_second_last l1, List.map snd l)
    | (l1,_)::(l2,_)::_ as l when Levels.at_most_one_after_last_dec t l1 && Levels.before_last_dec t l2 ->
      let l1 = Levels.merge l1 l2 in
      (Levels.get_second_last l1, List.map snd l)
    | (l1,Trail.Pcon.PCon(con,c))::lcon ->
      let f (type a) exp (e:a) =
        let module Exp = (val (ExpRegistry.get exp) : Exp with type t = a) in
        Exp.analyse t e con c
      in
      let age = Levels.get_last l1 in
      let (Trail.Pexp.Pexp(_,exp,e)) = Trail.get_pexp t age in
      let lcon' = f exp e in
      let lcon = merge_lcon (sort_lcon (convert_lcon t lcon')) lcon in
      aux lcon
  in
  let backtrack_level, l = aux lcon in
  let l = lcon_to_node l in
  backtrack_level, !_or l
