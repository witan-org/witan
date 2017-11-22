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

open Stdlib

(** {2 General caml types } *)

exception BrokenInvariant of string
exception SolveSameRepr
exception UnwaitedEvent
exception AlreadyDead
exception AlreadyRedirected

(** the key shouldn't be used before its registration and shouldn't be
    registered again *)
exception UnregisteredKey
exception AlreadyRegisteredKey

module Sem: Keys.Key
module Value: Keys.Key

type 'a sem = 'a Sem.t

module type Sem = sig
  include Datatype

  val key: t sem
end

val get_sem: 'a sem -> (module Sem with type t = 'a)
val sem_uninitialized: 'a sem -> bool
val check_sem_registered: 'a sem -> unit
val print_sem : 'a sem -> 'a Pp.pp

type 'a value = 'a Value.t

module type Value = sig
  include Datatype

  val key: t value
end

val get_value: 'a value -> (module Value with type t = 'a)
val value_uninitialized: 'a value -> bool
val check_value_registered: 'a value -> unit
val print_value : 'a value -> 'a Pp.pp

module Dem: Keys.Key2
type ('k,'d) dem = ('k,'d) Dem.t

(** Classes *)
module Node : sig
  include Datatype

  val fresh: ?to_reg:(('event,'r) dem * 'event) -> string -> Ty.t -> t
  (** the string is used as the prefix for the debug output *)

  val rename: t -> string -> unit
  (** to use with care *)

  val ty: t -> Ty.t

  val index: 'a sem -> 'a -> Ty.t -> t
  (** Return the corresponding node from a semantical term *)
end

module NodeSem: sig
  include Datatype


  val index: 'a sem -> 'a -> Ty.t -> t
  (** Return the corresponding node from a semantical term *)

  val node: t -> Node.t

  val ty: t -> Ty.t

end

module type RegisteredSem = sig
  type s
  val key: s sem

  (** nodesem *)
  include Datatype

  val index: s -> Ty.t -> t
  (** Return a nodesem from a semantical term *)

  val node: t -> Node.t
  (** Return a class from a nodesem *)

  val ty: t -> Ty.t
  (** Return the type from a nodesem *)

  val sem: t -> s
  (** Return the sem from a nodesem *)

  val nodesem: t -> NodeSem.t
  val of_nodesem: NodeSem.t -> t option

  val coerce_nodesem: NodeSem.t -> t

end


module RegisterSem (D:Sem) : RegisteredSem with type s = D.t


module NodeValue: sig
  include Datatype


  val index: 'a value -> 'a -> Ty.t -> t
  (** Return the corresponding node from a value *)

  val node: t -> Node.t

  val ty: t -> Ty.t

end

module type RegisteredValue = sig
  type s
  val key: s value

  (** nodevalue *)
  include Datatype

  val index: s -> Ty.t -> t
  (** Return a nodevalue from a valueantical term *)

  val node: t -> Node.t
  (** Return a class from a nodevalue *)

  val ty: t -> Ty.t
  (** Return the type from a nodevalue *)

  val value: t -> s
  (** Return the value from a nodevalue *)

  val nodevalue: t -> NodeValue.t
  val of_nodevalue: NodeValue.t -> t option

  val coerce_nodevalue: NodeValue.t -> t

end

module RegisterValue (D:Value) : RegisteredValue with type s = D.t



module Print : sig (** Cutting the knot for pp *)
  type pdem_event = { mutable
      pdem_event : 'k 'd. ('k,'d) dem -> 'k Pp.pp}

  val pdem_event : pdem_event
  val dem_event : ('k,'d) dem -> 'k Pp.pp

  type pdem_runable =
    { mutable pdem_runable : 'k 'd. ('k,'d) dem -> 'd Pp.pp}

  val pdem_runable : pdem_runable
  val dem_runable : ('k,'d) dem -> 'd Pp.pp


end

val check_initialization: unit -> bool
(** Check if the initialization of all the dom, sem and dem have been done *)

(** Only for Solver *)
module Only_for_solver: sig
  type sem_of_node =
    | Sem: 'a sem * 'a -> sem_of_node

  val nodesem:
    Node.t -> NodeSem.t option
    (** give the sem associated with a node, make sense only for not merged
        class. So only the module solver can use it *)

  val sem_of_node:
    NodeSem.t -> sem_of_node
    (** give the sem associated with a node, make sense only for not merged
        class. So only the module solver can use it *)

  type value_of_node =
    | Value: 'a value * 'a -> value_of_node

  val nodevalue:
    Node.t -> NodeValue.t option
    (** give the value associated with a node, make sense only for not merged
        class. So only the module solver can use it *)

  val value_of_node:
    NodeValue.t -> value_of_node
    (** give the value associated with a node, make sense only for not merged
        class. So only the module solver can use it *)

  val node_of_nodesem: NodeSem.t -> Node.t
  val node_of_nodevalue: NodeValue.t -> Node.t

  type opened_node =
    | Fresh: opened_node
    | Fresh_to_reg: ('event,'r) dem * 'event -> opened_node
    | Sem  : NodeSem.t -> opened_node
    | Value  : NodeValue.t -> opened_node

  val open_node: Node.t -> opened_node

end
