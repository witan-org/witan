(**********************************************************************)
(*  Copyright (C) Jean-Christophe Filliatre                           *)
(*                                                                    *)
(*  This software is free software; you can redistribute it and/or    *)
(*  modify it under the terms of the GNU Library General Public       *)
(*  License version 2.1, with the special exception on linking        *)
(*  described in file LICENSE.                                        *)
(*                                                                    *)
(*  This software is distributed in the hope that it will be useful,  *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of    *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.              *)
(**********************************************************************)

(* Leftist heaps.

   See for instance Chris Okasaki's "Purely Functional Data Structures" *)

(* Modified for adding db by François Bobot *)

module type Ordered = sig
  type db
  type prio
  type t

  val reprio: db -> t -> prio
  val le: t -> prio -> t -> prio -> bool
end

exception Empty

module Make(X : Ordered) =
struct

  type t = E | T of int * X.t * X.prio * t * t

  let rank = function E -> 0 | T (r,_,_,_,_) -> r

  let make x xprio a b =
    let ra = rank a and rb = rank b in
    if ra >= rb then T (rb + 1, x, xprio, a, b) else T (ra + 1, x, xprio, b, a)

  let empty = E

  let is_empty = function E -> true | T _ -> false

  let rec merge h1 h2 = match h1,h2 with
    | E, h | h, E ->
	h
    | T (_,x,xprio,a1,b1), T (_,y,yprio,a2,b2) ->
      if X.le x xprio y yprio
      then make x xprio a1 (merge b1 h2) else make y yprio a2 (merge h1 b2)

  let insert x prio h = merge (T (1, x, prio, E, E)) h

  let min = function E -> None | T (_,x,_,_,_) -> Some x

  let extract_min = function
    | E -> raise Empty
    | T (_,x,_,a,b) -> x, merge a b

  let rec reprio db = function
    | E -> E
    | T (_,x,_,a,b) ->
      let a = reprio db a in
      let b = reprio db b in
      let xprio = X.reprio db x in
      match a,b with
      | E,E -> T(1,x,xprio,a,b)
      | (T(_,ax,aprio,_,_) as h), E | E, (T(_,ax,aprio,_,_) as h)
        when X.le x xprio ax aprio -> make x xprio E h
      | T(_,ax,aprio,_,_), T(_,bx,bprio,_,_)
        when X.le x xprio ax aprio && X.le x xprio bx bprio ->
        make x xprio a b
      | _ -> merge a (insert x xprio b)

  let insert db x h = insert x (X.reprio db x) h

  let rec fold f acc = function
    | E -> acc
    | T (_,x,p,t1,t2) ->
      fold f (fold f (f acc x p) t1) t2
end
