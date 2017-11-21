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

(* Leftist heaps *)

module type Ordered = sig
  type db
  type prio
  type t

  val reprio: db -> t -> prio
  val le: t -> prio -> t -> prio -> bool
end

exception Empty

module Make(X: Ordered) :
sig
  type t

  val empty: t

  val is_empty: t -> bool
    (* runs in O(1) *)

  val insert: X.db -> X.t -> t -> t
    (* runs in O(log n),
       the db is just used for the priority of the new element *)

  val min: t -> X.t option
    (* runs in O(1) *)

  val extract_min: t -> X.t * t
    (* runs in O(log n) *)

  val merge: t -> t -> t
    (* runs in O(log max(n1, n2)) *)

  val reprio : X.db -> t -> t
    (* runs in O(n*n) said otherwise O(n*c) with c the number of change *)

  val fold : ('a -> X.t -> X.prio -> 'a) -> 'a -> t -> 'a
end
