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

module type S1 = sig
  type 'a key
  type ('a,'b) data
  type 'b t

  val create : int -> 'b t

  val size : 'b t -> int
  val get  : 'b t -> 'a key -> ('a,'b) data
  val set  : 'b t -> 'a key -> ('a,'b) data -> unit

  val is_uninitialized : 'b t -> 'a key -> bool
  val uninitialize     : 'b t -> 'a key -> unit

  val clear: 'b t -> unit

  val inc_size : 'a key -> 'b t -> unit

  type 'b iter_initialized = { iter: 'a. ('a,'b) data -> unit }
  val iter_initialized : 'b iter_initialized -> 'b t -> unit

  type ('b,'c) fold_initialized = { fold: 'a. 'c -> ('a,'b) data -> 'c }
  val fold_initialized :
     ('b,'c) fold_initialized -> 'c -> 'b t -> 'c

  type 'b iter_initializedi = { iteri: 'a. 'a key -> ('a,'b) data -> unit }
  val iter_initializedi :
    'b iter_initializedi -> 'b t -> unit

  type ('b,'c) fold_initializedi =
    { foldi: 'a. 'c -> 'a key -> ('a,'b) data -> 'c }
  val fold_initializedi :
     ('b,'c) fold_initializedi -> 'c -> 'b t -> 'c

  val copy : 'b t -> 'b t
  (* shallow *)

  type printk = { printk: 'a. 'a key Pp.pp }
  type 'b printd = { printd: 'a. 'a key -> ('a,'b) data Pp.pp }
  val pp:
   unit Pp.pp ->
   unit Pp.pp ->
   printk ->
   'b printd ->
   'b t Pp.pp

end

module Make1
  (K:sig type 'a t = private int end)
  (D:sig type ('a,'b) t end)
  = struct

  type 'a key = 'a K.t
  type ('a,'b) data = ('a,'b) D.t
  type exi
  type 'b t = (exi,'b) data Simple_vector.t

  let open_t (type a) (type b)  (t : b t) (_ : a key) :
      (a,b) D.t Simple_vector.t = Obj.magic t

  let create = Simple_vector.create

  let size = Simple_vector.size

  let set (type a) t (k : a K.t) d =
    let t = open_t t k in
    Simple_vector.set t (k :> int) d

  let get (type a) t (k : a K.t) =
    let t = open_t t k in
    Simple_vector.get t (k :> int)

  let is_uninitialized (type a) t (k : a K.t) =
    let t = open_t t k in
    Simple_vector.size t <= (k :> int) ||
      Simple_vector.is_uninitialized t (k :> int)

  let uninitialize (type a) t (k : a K.t) =
    let t = open_t t k in
    Simple_vector.uninitialize t (k :> int)

  let clear = Simple_vector.clear

  let inc_size (type a) (k : a K.t) t =
    Simple_vector.inc_size ((k :> int) + 1) t

  type 'b iter_initialized = { iter: 'a. ('a,'b) data -> unit }
  let iter_initialized f v = Simple_vector.iter_initialized f.iter v

  type ('b,'c) fold_initialized = { fold: 'a. 'c -> ('a,'b) data -> 'c }
  let fold_initialized f acc v =
    Simple_vector.fold_initialized f.fold acc v

  type 'b iter_initializedi = { iteri: 'a. 'a key -> ('a,'b) data -> unit }
  let iter_initializedi f t =
    Simple_vector.iter_initializedi (fun i d ->
      f.iteri
        (Obj.magic (i : int) :> exi K.t) (* at some time this key have been
                                            given or it must be not
                                            initialized *)
        d) t

  type ('b,'c) fold_initializedi =
    { foldi: 'a. 'c -> 'a key -> ('a,'b) data -> 'c }

  let fold_initializedi f acc t =
    Simple_vector.fold_initializedi (fun acc i d ->
      f.foldi acc
        (Obj.magic (i : int) :> exi K.t) (* same thing than for iteri *)
        d) acc t

  let copy = Simple_vector.copy
  (* shallow *)

  type printk = { printk: 'a. 'a key Pp.pp }
  type 'b printd = { printd: 'a. 'a key -> ('a,'b) data Pp.pp }
  let pp sep1 sep2 printkey printdata fmt t =
    let printkey fmt i =
      printkey.printk fmt
        (Obj.magic (i : int) : exi K.t) (* same thing than for iteri *)
    in
    let printdata i fmt d =
      printdata.printd
        (Obj.magic (i : int) : exi K.t) (* same thing than for iteri *)
        fmt d
    in
    Pp.iteri2 Simple_vector.iter_initializedi
      sep1 sep2 printkey printdata fmt t

end


module type S2 = sig
  type ('a1,'a2) key
  type ('a1,'a2,'b) data
  type 'b t

  val create : int -> 'b t

  val size : 'b t -> int
  val get  : 'b t -> ('a1,'a2) key -> ('a1,'a2,'b) data
  val set  : 'b t -> ('a1,'a2) key -> ('a1,'a2,'b) data -> unit

  val is_uninitialized : 'b t -> ('a1,'a2) key -> bool
    (** Contrary to Simple_vector it tests the size too *)
  val uninitialize     : 'b t -> ('a1,'a2) key -> unit

  val clear: 'b t -> unit

  val inc_size : ('a1,'a2) key -> 'b t -> unit

  type 'b iter_initialized = { iter: 'a1 'a2. ('a1, 'a2, 'b) data -> unit }
  val iter_initialized : 'b iter_initialized -> 'b t -> unit

  type ('b,'c) fold_initialized =
    { fold: 'a1 'a2. 'c -> ('a1,'a2,'b) data -> 'c }
  val fold_initialized :
     ('b,'c) fold_initialized -> 'c -> 'b t -> 'c

  type 'b iter_initializedi =
    { iteri: 'a1 'a2. ('a1,'a2) key -> ('a1,'a2,'b) data -> unit }
  val iter_initializedi :
    'b iter_initializedi -> 'b t -> unit

  type ('b,'c) fold_initializedi =
    { foldi: 'a1 'a2. 'c -> ('a1,'a2) key -> ('a1,'a2,'b) data -> 'c }
  val fold_initializedi :
     ('b,'c) fold_initializedi -> 'c -> 'b t -> 'c

  val copy : 'b t -> 'b t
  (* shallow *)
end

module Make2
  (K:sig type ('a1,'a2) t = private int end)
  (D:sig type ('a1,'a2,'b) t end)
  = struct

  type ('a1,'a2) key = ('a1,'a2) K.t
  type ('a1,'a2,'b) data = ('a1,'a2,'b) D.t
  type exi1
  type exi2
  type 'b t = (exi1,exi2,'b) data Simple_vector.t

  let open_t (type a1) (type a2) (type b)  (t : b t) (_ : (a1,a2) key) :
      (a1,a2,b) D.t Simple_vector.t = Obj.magic t

  let create = Simple_vector.create

  let size = Simple_vector.size

  let set (type a1) (type a2) t (k : (a1,a2) K.t) d =
    let t = open_t t k in
    Simple_vector.set t (k :> int) d

  let get (type a1) (type a2) t (k : (a1,a2) K.t) =
    let t = open_t t k in
    Simple_vector.get t (k :> int)

  let is_uninitialized (type a1) (type a2) t (k : (a1,a2) K.t) =
    let t = open_t t k in
    Simple_vector.size t <= (k :> int) ||
      Simple_vector.is_uninitialized t (k :> int)

  let uninitialize (type a1) (type a2) t (k : (a1,a2) K.t) =
    let t = open_t t k in
    Simple_vector.uninitialize t (k :> int)

  let clear = Simple_vector.clear

  let inc_size (type a1) (type a2) (k : (a1,a2) K.t) t =
    Simple_vector.inc_size ((k :> int) + 1) t

  type 'b iter_initialized = { iter: 'a1 'a2. ('a1, 'a2, 'b) data -> unit }
  let iter_initialized f v = Simple_vector.iter_initialized f.iter v

  type ('b,'c) fold_initialized =
    { fold: 'a1 'a2. 'c -> ('a1,'a2,'b) data -> 'c }
  let fold_initialized f acc v =
    Simple_vector.fold_initialized f.fold acc v

  type 'b iter_initializedi =
    { iteri: 'a1 'a2. ('a1,'a2) key -> ('a1,'a2,'b) data -> unit }
  let iter_initializedi f t =
    Simple_vector.iter_initializedi (fun i d ->
      f.iteri (Obj.magic (i : int) :> (exi1,exi2) K.t)
        (* same thing than for Make1.iteri *)
        d) t

  type ('b,'c) fold_initializedi =
    { foldi: 'a1 'a2. 'c -> ('a1,'a2) key -> ('a1,'a2,'b) data -> 'c }
  let fold_initializedi f acc t =
    Simple_vector.fold_initializedi (fun acc i d ->
      f.foldi acc
        (Obj.magic (i : int) :> (exi1,exi2) K.t) (* same thing than for iteri *)
        d) acc t


  let copy = Simple_vector.copy
  (* shallow *)

end

(** Same as S1 but for ('a,'b) data = 'b *)
module type R1 = sig
  type 'a key
  type 'b t

  val create : int -> 'b t

  val size : 'b t -> int
  val get  : 'b t -> 'a key -> 'b
  val set  : 'b t -> 'a key -> 'b -> unit

  val is_uninitialized : 'b t -> 'a key -> bool
    (** Contrary to Simple_vector it tests the size too *)

  val inc_size : 'a key -> 'b t -> unit

  val iter_initialized : ('b -> unit) -> 'b t -> unit

  val fold_initialized : ('c -> 'b -> 'c) -> 'c -> 'b t -> 'c

  val apply_initialized : ('b -> 'b) -> 'b t -> unit

  type 'b iter_initializedi = { iteri: 'a. 'a key -> 'b -> unit }
  val iter_initializedi :
    'b iter_initializedi -> 'b t -> unit

  type ('b,'c) fold_initializedi =
    { foldi: 'a. 'c -> 'a key -> 'b -> 'c }
  val fold_initializedi :
     ('b,'c) fold_initializedi -> 'c -> 'b t -> 'c

  val copy : 'b t -> 'b t
  (* shallow *)


  type printk = { printk: 'a. 'a key Pp.pp }
  val pp:
    unit Pp.pp ->
    unit Pp.pp ->
    printk ->
    'b Pp.pp ->
    'b t Pp.pp

end

module RMake1 (K:sig type 'a t = private int end) = struct
  include Make1 (K) (struct type ('a,'b) t = 'b end)

  let iter_initialized f v = Simple_vector.iter_initialized f v

  let fold_initialized f acc v = Simple_vector.fold_initialized f acc v

  let apply_initialized f v = Simple_vector.apply_initialized f v

  let pp sep1 sep2 printkey pp fmt t =
    let printkey fmt i =
      printkey.printk fmt
        (Obj.magic (i : int) :> exi K.t) (* same thing than for iteri *)
    in
    Pp.iter2 Simple_vector.iter_initializedi
      sep1 sep2 printkey pp fmt t


end


(** Same as S1 but for ('a,'b) data = 'b *)
module type T1 = sig
  type 'a key
  type 'b t (* used only with 'b = unit *)

  val create : int -> unit t

  val size : unit t -> int
  val get  : unit t -> 'a key -> 'a
  val set  : unit t -> 'a key -> 'a -> unit

  val is_uninitialized : unit t -> 'a key -> bool
    (** Contrary to Simple_vector it tests the size too *)

  val inc_size : 'a key -> unit t -> unit

  type 'b iter_initializedi = { iteri: 'a. 'a key -> 'a -> unit }
  val iter_initializedi :
    unit iter_initializedi -> unit t -> unit

  type ('b,'c) fold_initializedi =
    { foldi: 'a. 'c -> 'a key -> 'a -> 'c }
  val fold_initializedi :
     (unit,'c) fold_initializedi -> 'c -> unit t -> 'c

  val copy : unit t -> unit t
  (* shallow *)

  type printk = { printk: 'a. 'a key Pp.pp }
  type 'b printd = { printd: 'a. 'a key -> 'a Pp.pp }
  val pp:
    unit Pp.pp ->
    unit Pp.pp ->
    printk ->
    unit printd ->
    unit t Pp.pp

end

module TMake1 (K:sig type 'a t = private int end) = struct
  include Make1 (K) (struct type ('a,'b) t = 'a end)

  let pp sep1 sep2 printkey printdata fmt t =
    let printkey fmt i =
      printkey.printk fmt
        (Obj.magic (i : int) : exi K.t) (* same thing than for iteri *)
    in
    let printdata i fmt d =
      printdata.printd
        (Obj.magic (i : int) : exi K.t) (* same thing than for iteri *)
        fmt d
    in
    Pp.iteri2 Simple_vector.iter_initializedi
      sep1 sep2 printkey printdata fmt t


end
