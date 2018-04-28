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

(** imperative, extensible and heterogene Arrays *)
module type S1 = sig
  type 'a key
  type ('a,'b) data
  type 'b t

  val create : int -> 'b t

  val size : 'b t -> int
  val get  : 'b t -> 'a key -> ('a,'b) data
  val get_def : 'b t -> 'a key -> ('a,'b) data -> ('a,'b) data
  val set  : 'b t -> 'a key -> ('a,'b) data -> unit

  val is_uninitialized : 'b t -> 'a key -> bool
    (** Contrary to Simple_vector it tests the size too *)
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

  val move: from:'b t -> to_:'b t -> unit

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
  : S1 with type 'a key = 'a K.t and type ('a,'b) data = ('a,'b) D.t

module type S2 = sig
  type ('a1,'a2) key
  type ('a1,'a2,'b) data
  type 'b t

  val create : int -> 'b t

  val size : 'b t -> int
  val get  : 'b t -> ('a1,'a2) key -> ('a1,'a2,'b) data
  val get_def  : 'b t -> ('a1,'a2) key -> ('a1,'a2,'b) data -> ('a1,'a2,'b) data
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

  val move: from:'b t -> to_:'b t -> unit

end

module Make2
  (K:sig type ('a1,'a2) t = private int end)
  (D:sig type ('a1,'a2,'b) t end)
  : S2 with type ('a1,'a2) key = ('a1,'a2) K.t
        and type ('a1,'a2,'b) data = ('a1,'a2,'b) D.t

(** The following are needed in order to avoid ('a,'b) t = 'b in an
    instanciation of the previous functors
    (cf. ocaml mantis #5083:
    J.Garrigue : "Phantom types must be either abstract or private.
    In particular, using an abbreviation for a phantom type is just
    a Russian roulette.")
*)

(** Same as S1 but for ('a,'b) data = 'b *)
module type R1 = sig
  type 'a key
  type 'b t

  val create : int -> 'b t

  val size : 'b t -> int
  val get  : 'b t -> 'a key -> 'b
  val get_def  : 'b t -> 'a key -> 'b -> 'b
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

  val move: from:'b t -> to_:'b t -> unit

  type printk = { printk: 'a. 'a key Pp.pp }
  val pp:
    unit Pp.pp ->
    unit Pp.pp ->
    printk ->
    'b Pp.pp ->
    'b t Pp.pp

end

module RMake1
  (K:sig type 'a t = private int end)
  : R1 with type 'a key = 'a K.t

(** Same as S1 but for ('a,'b) data = 'a *)
module type T1 = sig
  type 'a key
  type 'b t (* used only with 'b = unit *)

  val create : int -> unit t

  val size : unit t -> int
  val get  : unit t -> 'a key -> 'a
  val get_def : unit t -> 'a key -> 'a -> 'a
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

  val move: from:unit t -> to_:unit t -> unit

  type printk = { printk: 'a. 'a key Pp.pp }
  type 'b printd = { printd: 'a. 'a key -> 'a Pp.pp }
  val pp:
    unit Pp.pp ->
    unit Pp.pp ->
    printk ->
    unit printd ->
    unit t Pp.pp

end

module TMake1
  (K:sig type 'a t = private int end)
  : T1 with type 'a key = 'a K.t
