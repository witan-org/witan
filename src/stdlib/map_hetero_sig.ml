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

exception IncoherentMap

module type Keys = sig
  type 'a t
  val tag : 'a t -> int
  val equal : 'a t -> 'b t -> ('a,'b) Poly.iseq
end

module type S = sig
  type 'a key
  type ('a,'b) data
  type 'b t

  val empty: 'b t
  val is_empty: 'b t -> bool
  val set_submap : 'a t -> 'b t -> bool

  val singleton: 'a key -> ('a,'b) data -> 'b t

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

  type ('b,'c) fold2_inter =
    { fold2_inter: 'a. 'a key -> ('a,'b) data -> ('a,'b) data -> 'c -> 'c }
  val fold2_inter: ('b,'c) fold2_inter -> 'b t -> 'b t -> 'c -> 'c

  type 'b iter = { iter: 'a. 'a key -> ('a,'b) data -> unit }
  val iter : 'b iter -> 'b t -> unit

  type ('b,'c) fold = { fold: 'a. 'c -> 'a key -> ('a,'b) data -> 'c }
  val fold : ('b,'c) fold -> 'c -> 'b t -> 'c

  type 'b mapi = { mapi: 'a. 'a key -> ('a,'b) data -> ('a,'b) data }
  val mapi : 'b mapi -> 'b t -> 'b t

  type printk = { printk: 'a. 'a key Format.printer }
  type 'b printd = { printd: 'a. 'a key -> ('a,'b) data Format.printer }
  val pp:
    unit Format.printer ->
    unit Format.printer ->
    printk ->
    'b printd ->
    'b t Format.printer

end


(** The following are needed in order to avoid ('a,'b) t = 'b in an
    instanciation of the previous functors
    (cf. ocaml mantis #5083:
    J.Garrigue : "Phantom types must be either abstract or private.
    In particular, using an abbreviation for a phantom type is just
    a Russian roulette.")
*)

(** Same as S but for ('a,'b) data = 'b *)
module type R = sig
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

  type 'b union = { union: 'a. 'a key -> 'b -> 'b -> 'b option }
  val union : 'b union -> 'b t -> 'b t -> 'b t

  type 'b iter = { iter: 'a. 'a key -> 'b -> unit }
  val iter : 'b iter -> 'b t -> unit

  type ('b,'c) fold = { fold: 'a. 'c -> 'a key -> 'b -> 'c }
  val fold : ('b,'c) fold -> 'c -> 'b t -> 'c

  type 'b mapi = { mapi: 'a. 'a key -> 'b -> 'b }
  val mapi : 'b mapi -> 'b t -> 'b t

  type printk = { printk : 'a. 'a key Containers.Format.printer; }
  val pp:
    unit Format.printer ->
    unit Format.printer ->
    printk ->
    'b Format.printer ->
    'b t Format.printer

end
