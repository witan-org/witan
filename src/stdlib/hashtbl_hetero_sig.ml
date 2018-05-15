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

open Std

exception IncoherentTable

module type Keys1 = sig
  type 'a t
  val tag : 'a t -> int
  val equal : 'a t -> 'b t -> ('a,'b) Poly.iseq
end

(** imperative, extensible and heterogene hash-tables *)

module type S1 = sig
  type 'a key
  type ('a,'b) data
  type 'b t

  val create  : int -> 'b t
  val size    : 'b t -> int
  val get     : 'b t -> 'a key -> ('a,'b) data
  val get_def : 'b t -> 'a key -> ('a,'b) data -> ('a,'b) data
  val set     : 'b t -> 'a key -> ('a,'b) data -> unit
  val is_uninitialized : 'b t -> 'a key -> bool
  (** Contrary to Simple_vector it tests the size too *)
  val uninitialize     : 'b t -> 'a key -> unit
  val clear    : 'b t -> unit
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

  type printk = { printk: 'a. 'a key Format.printer }
  type 'b printd = { printd: 'a. 'a key -> ('a,'b) data Format.printer }
  val pp:
    unit Format.printer ->
    unit Format.printer ->
    printk ->
    'b printd ->
    'b t Format.printer

end

(** Same as S1 but for ('a,'b) data = 'b *)
module type R1 = sig
  (* type ('a,'b) data = 'b *)
  include S1 with type ('a,'b) data = 'b
  (* Some primitives get their types simplified *)
  val iter_initialized  : ('b -> unit) -> 'b t -> unit
  val fold_initialized  : ('c -> 'b -> 'c) -> 'c -> 'b t -> 'c
  val apply_initialized : ('b -> 'b) -> 'b t -> unit
  val pp:
    unit Format.printer ->
    unit Format.printer ->
    printk ->
    'b Format.printer ->
    'b t Format.printer
end

(** Same as S1 but for ('a,'b) data = 'a *)
module type T1 = sig
  type t
  type 'a key
  val create : int -> t
  val size : t -> int
  val get : t -> 'a key -> 'a
  val get_def : t -> 'a key -> 'a -> 'a
  val set : t -> 'a key -> 'a -> unit
  val is_uninitialized : t -> 'a key -> bool
  val uninitialize : t -> 'a key -> unit
  val clear : t -> unit
  val inc_size : 'a key -> t -> unit
  type iter_initialized = { iter : 'a. 'a -> unit; }
  val iter_initialized : iter_initialized -> t -> unit
  type 'c fold_initialized = { fold : 'a. 'c -> 'a -> 'c; }
  val fold_initialized : 'c fold_initialized -> 'c -> t -> 'c
  type iter_initializedi = { iteri : 'a. 'a key -> 'a -> unit; }
  val iter_initializedi : iter_initializedi -> t -> unit
  type 'c fold_initializedi = { foldi : 'a. 'c -> 'a key -> 'a -> 'c; }
  val fold_initializedi : 'c fold_initializedi -> 'c -> t -> 'c
  val copy : t -> t
  val move : from: t -> to_: t -> unit
  type printk = { printk : 'a. 'a key Containers.Format.printer; }
  type printd = { printd : 'a. 'a key -> 'a Containers.Format.printer; }
  val pp :
    unit Containers.Format.printer ->
    unit Containers.Format.printer ->
    printk -> printd -> t Containers.Format.printer
end


module type Keys2 = sig
  type ('a1,'a2) t
  val tag : ('a1,'a2) t -> int
  val equal : ('a1,'a2) t -> ('b1,'b2) t -> ('a1*'a2,'b1*'b2) Poly.iseq
end

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
