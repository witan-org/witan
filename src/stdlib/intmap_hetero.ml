(*************************************************************************)
(*                                                                       *)
(*  This file is part of Frama-C.                                        *)
(*                                                                       *)
(*  Copyright (C) 2007-2017                                              *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies             *)
(*         alternatives)                                                 *)
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
(*                                                                       *)
(*************************************************************************)

open Stdlib

module type S1 = sig
  type 'a key
  type ('a,'b) data
  type 'b t

  val empty: 'b t
  val is_empty: 'b t -> bool
  val set_submap : 'a t -> 'b t -> bool

  val singleton: 'a key -> 'b -> 'b t

  val find: 'a key -> 'b t -> ('a,'b) data
  (** [find x m] returns the current binding of [x] in [m],
      or raises [Not_found] if no such binding exists. *)

  val find_def : ('a,'b) data -> 'a key -> 'b t -> ('a,'b) data
  val find_opt : 'a key -> 'b t -> ('a,'b) data option
  val find_exn : exn -> 'a key -> 'b t -> ('a,'b) data

  val add: 'a key -> ('a,'b) data -> 'b t -> 'b t
  (** [add x y m] returns a map containing the same bindings as
      [m], plus a binding of [x] to [y]. If [x] was already bound
      in [m], its previous binding disappears. *)

  val change :
    (('a,'b) data option -> ('a,'b) data option) -> 'a key -> 'b t -> 'b t
  val add_change :
    ('c -> ('a,'b) data) ->
    ('c -> ('a,'b) data -> ('a,'b) data) ->
    'a key -> 'c -> 'b t -> 'b t

  type 'b union =
    { union: 'a. 'a key -> ('a,'b) data -> ('a,'b) data -> ('a,'b) data option }
  val union : 'b union -> 'b t -> 'b t -> 'b t

  type 'b iter = { iter: 'a. 'a key -> ('a,'b) data -> unit }
  val iter : 'b iter -> 'b t -> unit

  type ('b,'c) fold = { fold: 'a. 'c -> 'a key -> ('a,'b) data -> 'c }
  val fold : ('b,'c) fold -> 'c -> 'b t -> 'c

  type 'b mapi = { mapi: 'a. 'a key -> ('a,'b) data -> ('a,'b) data }
  val mapi : 'b mapi -> 'b t -> 'b t

  type 'b pp = { pp: 'a. ('a,'b) data Pp.pp }
  val pp:
    (unit Pp.pp) ->
    'b pp ->
    'b t Pp.pp

end


module Make1
  (K:sig type 'a t = private int end)
  (D:sig type ('a,'b) t end)
  = struct

  type 'a key = 'a K.t
  type ('a,'b) data = ('a,'b) D.t
  type exi
  type 'b t = (exi,'b) data DInt.M.t

  let open_t (type a) (type b)  (t : b t) (_ : a key) :
      (a,b) D.t DInt.M.t = Obj.magic t

  let empty = DInt.M.empty
  let is_empty = DInt.M.is_empty
  let set_submap = DInt.M.set_submap

  let singleton (type a) (type b) (k: a K.t) (d : b) : b t =
    (Obj.magic (DInt.M.singleton (k :> int) d) : b t)

  let add (type a) (type b) (k : a K.t) d (t : b t) =
    let t = open_t t k in
    (Obj.magic (DInt.M.add (k :> int) d t) : b t)

  let change (type a) (type b) f (k : a K.t) (t : b t) =
    let t = open_t t k in
    (Obj.magic (DInt.M.change f (k :> int) t) : b t)

  let add_change (type a) (type b) empty add (k : a K.t) v (t : b t) =
    let t = open_t t k in
    (Obj.magic (DInt.M.add_change empty add (k :> int) v t) : b t)

  let find (type a) (k : a K.t) t =
    let t = open_t t k in
    DInt.M.find (k :> int) t

  let find_def (type a) def (k : a K.t) t =
    let t = open_t t k in
    DInt.M.find_def def (k :> int) t

  let find_opt (type a) (k : a K.t) t =
    let t = open_t t k in
    DInt.M.find_opt (k :> int) t

  let find_exn (type a) exn (k : a K.t) t =
    let t = open_t t k in
    DInt.M.find_exn exn (k :> int) t


  type 'b union =
    { union: 'a. 'a key -> ('a,'b) data -> ('a,'b) data -> ('a,'b) data option }
  let union f t1 t2 =
    DInt.M.union (fun i d1 d2 ->
      f.union
        (Obj.magic (i : int) :> exi K.t) (* at some time this key have been
                                            given *)
        d1 d2) t1 t2

  type 'b iter = { iter: 'a. 'a key -> ('a,'b) data -> unit }
  let iter f t =
    DInt.M.iter (fun i d ->
      f.iter
        (Obj.magic (i : int) :> exi K.t) (* at some time this key have been
                                            given or it must be not
                                            initialized *)
        d) t

  type ('b,'c) fold = { fold: 'a. 'c -> 'a key -> ('a,'b) data -> 'c }

  let fold f acc t =
    DInt.M.fold_left (fun acc i d ->
      f.fold acc
        (Obj.magic (i : int) :> exi K.t) (* same thing than for iter *)
        d) acc t


  type 'b mapi = { mapi: 'a. 'a key -> ('a,'b) data -> ('a,'b) data }
  let mapi f t =
    DInt.M.mapi (fun i d ->
      f.mapi
        (Obj.magic (i : int) :> exi K.t) (* same thing than for iter *)
        d) t

  type 'b pp = { pp: 'a. ('a,'b) data Pp.pp }
  let pp sep pp fmt t =
    Pp.iter2 DInt.M.iter Pp.nothing sep Pp.nothing pp.pp fmt t

end

module type R1 = sig
  type 'a key
  type 'b t

  val empty: 'b t
  val is_empty: 'b t -> bool
  val set_submap : 'a t -> 'b t -> bool

  val singleton: 'a key -> 'b -> 'b t

  val find: 'a key -> 'b t -> 'b
  (** [find x m] returns the current binding of [x] in [m],
      or raises [Not_found] if no such binding exists. *)

  val find_def : 'b -> 'a key -> 'b t -> 'b
  val find_opt : 'a key -> 'b t -> 'b option
  val find_exn : exn -> 'a key -> 'b t -> 'b

  val add: 'a key -> 'b -> 'b t -> 'b t
  (** [add x y m] returns a map containing the same bindings as
      [m], plus a binding of [x] to [y]. If [x] was already bound
      in [m], its previous binding disappears. *)

  val change :
    ('b option -> 'b option) -> 'a key -> 'b t -> 'b t
  val add_change :
    ('c -> 'b) ->
    ('c -> 'b -> 'b) ->
    'a key -> 'c -> 'b t -> 'b t

  type 'b union =
    { union: 'a. 'a key -> 'b -> 'b -> 'b option }
  val union : 'b union -> 'b t -> 'b t -> 'b t

  type 'b iter = { iter: 'a. 'a key -> 'b -> unit }
  val iter : 'b iter -> 'b t -> unit

  type ('b,'c) fold = { fold: 'a. 'c -> 'a key -> 'b -> 'c }
  val fold : ('b,'c) fold -> 'c -> 'b t -> 'c

  type 'b mapi = { mapi: 'a. 'a key -> 'b -> 'b }
  val mapi : 'b mapi -> 'b t -> 'b t

  type printk = { printk: 'a. 'a key Pp.pp }
  val pp:
    (unit Pp.pp) ->
    (unit Pp.pp) ->
    printk ->
    ('b Pp.pp) ->
    'b t Pp.pp

end

module RMake1 (K:sig type 'a t = private int end) = struct
  include Make1(K)(struct type ('a,'b) t = 'b end)

  type printk = { printk: 'a. 'a key Pp.pp }

  let pp sep1 sep2 printkey pp fmt (t : 'b t) =
    let printkey fmt i =
      printkey.printk fmt
        (Obj.magic (i : int) :> exi K.t) (* same thing than for iteri *)
    in
    Pp.iter2 DInt.M.iter
      sep1 sep2 printkey pp fmt t

end
