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

open Witan_core
open Witan_stdlib.Std

module T = struct
  type t = { cst : Q.t; poly : Q.t Node.M.t}

  let equal n m = Q.equal n.cst m.cst && Node.M.equal Q.equal n.poly m.poly

  let hash n = (** number au pif *)
    Node.M.fold (fun k v acc ->
        Node.hash k * 101 + Hashtbl.hash v * 107 + acc * 253)
      n.poly (Hashtbl.hash n.cst * 27)

  let compare n m =
    let c = Q.compare n.cst m.cst in
    if c <> 0 then c
    else Node.M.compare Q.compare n.poly m.poly

  let pp fmt v =
    let print_not_1 first fmt q =
      if not first && Q.compare q Q.zero >= 0
      then Format.pp_print_string fmt "+";
      if Q.equal q Q.zero then  Format.pp_print_string fmt "!0!"
      else if Q.equal Q.minus_one q then Format.pp_print_string fmt "-"
      else if not (Q.equal Q.one q) then Q.pp fmt q
    in
    let print_not_0 first fmt q =
      if first
      then Q.pp fmt q
      else
      if not (Q.equal Q.zero q) then begin
        if Q.compare q Q.zero > 0 then Format.pp_print_string fmt "+";
        Q.pp fmt q
      end
    in
    let print_mono k v (fmt,first) =
      Format.fprintf fmt "@[%a%a@]@," (print_not_1 first) v Node.pp k;
      (fmt,false)
    in
    Format.fprintf fmt "@[";
    let _,first = Node.M.fold print_mono v.poly (fmt,true) in
    Format.fprintf fmt "%a@]" (print_not_0 first) v.cst

end

include T
include Stdlib.MkDatatype(T)

(** different invariant *)

let invariant p =
  Node.M.for_all (fun _ q -> not (Q.equal q Q.zero)) p.poly

(** constructor *)
let cst q = {cst = q; poly = Node.M.empty}
let zero = cst Q.zero
let is_cst p = if Node.M.is_empty p.poly then Some p.cst else None
let is_zero p = Q.equal p.cst Q.zero && Node.M.is_empty p.poly

type extract = Zero | Cst of Q.t | Var of Q.t * Node.t * t
let extract p =
  if Node.M.is_empty p.poly then
    if Q.equal p.cst Q.zero then Zero
    else Cst p.cst
  else
    let x,q = Shuffle.chooseb Node.M.choose Node.M.choose_rnd p.poly in
    let p' = {p with poly = Node.M.remove x p.poly} in
    Var(q,x,p')

type kind = ZERO | CST | VAR
let classify p =
  if Node.M.is_empty p.poly then
    if Q.equal p.cst Q.zero then ZERO
    else CST
  else
    VAR


let monome c x =
  if Q.equal Q.zero c then cst Q.zero
  else {cst = Q.zero; poly = Node.M.singleton x c}

let is_one_node p = (** cst = 0 and one empty monome *)
  if Q.equal Q.zero p.cst && Node.M.is_num_elt 1 p.poly then
    let node,k = Node.M.choose p.poly in
    if Q.equal Q.one k then Some node
    else None
  else None

let sub_cst p q = {p with cst = Q.sub p.cst q}

let mult_cst c p1 =
  if Q.equal Q.one c then p1
  else
  let poly_mult c m = Node.M.map (fun c1 -> Q.mul c c1) m in
  if Q.equal Q.zero c then cst Q.zero
  else {cst = Q.mul c p1.cst; poly = poly_mult c p1.poly}


let none_zero c = if Q.equal Q.zero c then None else Some c

(** Warning Node.M.union can be used only for defining an operation [op]
    that verifies [op 0 p = p] and [op p 0 = p] *)
let add p1 p2 =
  let poly_add m1 m2 =
    Node.M.union (fun _ c1 c2 -> none_zero (Q.add c1 c2)) m1 m2
  in
  {cst = Q.add p1.cst p2.cst; poly = poly_add p1.poly p2.poly}

let sub p1 p2 =
  let poly_sub m1 m2 =
    Node.M.union_merge (fun _ c1 c2 ->
      match c1 with
      | None -> Some (Q.neg c2)
      | Some c1 -> none_zero (Q.sub c1 c2))
      m1 m2 in
  {cst = Q.sub p1.cst p2.cst; poly = poly_sub p1.poly p2.poly}

let x_p_cy p1 c p2 =
  assert (not (Q.equal c Q.zero));
  let f a b = Q.add a (Q.mul c b) in
  let poly m1 m2 =
    Node.M.union_merge (fun _ c1 c2 ->
      match c1 with
      | None -> Some (Q.mul c c2)
      | Some c1 -> none_zero (f c1 c2))
      m1 m2 in
  {cst = f p1.cst p2.cst; poly = poly p1.poly p2.poly}


let cx_p_cy c1 p1 c2 p2 =
  let c1_iszero = Q.equal c1 Q.zero in
  let c2_iszero = Q.equal c2 Q.zero in
  if c1_iszero && c2_iszero then zero
  else if c1_iszero
  then p2
  else if c2_iszero
  then p1
  else
    let f e1 e2 = Q.add (Q.mul c1 e1) (Q.mul c2 e2) in
    let poly m1 m2 =
      Node.M.merge (fun _ e1 e2 ->
          match e1, e2 with
          | None, None -> assert false
          | None, Some e2 -> Some (Q.mul c2 e2)
          | Some e1, None -> Some (Q.mul c1 e1)
          | Some e1, Some e2 ->
            none_zero (f e1 e2))
        m1 m2 in
    {cst = f p1.cst p2.cst; poly = poly p1.poly p2.poly}

let subst_node p x y =
  let poly,qo = Node.M.find_remove x p.poly in
  match qo with
  | None -> p, Q.zero
  | Some q ->
    let poly = Node.M.change (function
        | None -> qo
        | Some q' -> none_zero (Q.add q q')
      ) y poly in
    {p with poly}, q

let subst p x s =
  let poly,q = Node.M.find_remove x p.poly in
  match q with
  | None -> p, Q.zero
  | Some q -> x_p_cy {p with poly} q s, q

let fold f acc p = Node.M.fold_left f acc p.poly
let iter f p = Node.M.iter f p.poly

let of_list cst l =
  let fold acc (node,q) = Node.M.change (function
      | None -> Some q
      | Some q' -> none_zero (Q.add q q')) node acc in
  {cst;poly= List.fold_left fold Node.M.empty l}

module ClM = Extmap.Make(Node)

type 'a tree = 'a ClM.view =
  | Empty
  | Node of 'a tree * Node.t * 'a * 'a tree * int

let get_tree p =
  (ClM.view
    (Node.M.fold_left (fun acc node q ->  ClM.add node q acc) ClM.empty p.poly))
, p.cst
