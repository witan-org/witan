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

(** Define the Node, and the related types semantic terms and value *)

open Stdlib

(** {2 General exceptions (to move away) } *)

exception BrokenInvariant of string
exception SolveSameRepr
exception UnwaitedEvent
exception AlreadyDead
exception AlreadyRedirected


module Sem: Keys.Key
module Value: Keys.Key
module Dem: Keys.Key2

(** Node *)

module Node : sig
  include Datatype

  val fresh: ?to_reg:(('event,'r) Dem.t * 'event) -> string -> Ty.t -> t
  (** The string is used as the prefix for the debug output. [?to_reg]
      allows to have an event emitted when this class is registered in
      a solver.
  *)

  val rename: t -> string -> unit
  (** Change the pretty printed string for this node, to use with care
      preferably at the start *)

  val ty: t -> Ty.t
  (** Return the type of a node *)

  val index_sem: 'a Sem.t -> 'a -> Ty.t -> t
  (** Return the corresponding node from a semantical term *)

  val index_value: 'a Value.t -> 'a -> Ty.t -> t
  (** Return the corresponding node from a value *)
end

(** {2 Semantical terms } *)

(** Basically a semantic terms is just something with an ordering. For
    each semantic terms a unique node is associated. *)
module type Sem = sig
  include Datatype

  val key: t Sem.t
end

(** {3 Generic Handling of semantical terms} *)

val get_sem: 'a Sem.t -> (module Sem with type t = 'a)
val sem_uninitialized: 'a Sem.t -> bool
val check_sem_registered: 'a Sem.t -> unit
val print_sem : 'a Sem.t -> 'a Pp.pp

module NodeSem: sig
  include Datatype


  val index: 'a Sem.t -> 'a -> Ty.t -> t
  (** Return the corresponding node from a semantical term *)

  val node: t -> Node.t
  (** Returns the node associated to this semantical term *)

  val ty: t -> Ty.t
  (** Returns the type of the semantical term *)

end

(** {3 Construction of a semantical terms } *)

(** Result of the registration of a semantical term *)
module type RegisteredSem = sig
  type s
  (** the user given type *)

  val key: s Sem.t

  (** nodesem *)
  include Datatype

  val index: s -> Ty.t -> t
  (** Return a semantical term from the user type *)

  val node: t -> Node.t
  (** Return a class from a nodesem *)

  val ty: t -> Ty.t
  (** Return the type from a nodesem *)

  val sem: t -> s
  (** Return the sem from a nodesem *)

  val nodesem: t -> NodeSem.t
  val of_nodesem: NodeSem.t -> t option
  (** Return the user type if the NodeSem belongs to this module *)

  val coerce_nodesem: NodeSem.t -> t
  (** Return the user type. Raise if the NodeSem does not belong to this
      module *)

end

module RegisterSem (D:Sem) : RegisteredSem with type s = D.t

(** {2 Values } *)

(** Basically a value is just something with an ordering. For each
    value a unique node is associated. The difference with semantic
    terms is that only one value of a kind can be in an equivalence
    class. The solver keep track of which value is associated to an
    equivalence class (like it does for domains) *)
module type Value = sig
  include Datatype

  val key: t Value.t
end

val print_value : 'a Value.t -> 'a Pp.pp
val get_value: 'a Value.t -> (module Value with type t = 'a)
val value_uninitialized: 'a Value.t -> bool
val check_value_registered: 'a Value.t -> unit

(** {3 Module for handling generically values} *)

module NodeValue: sig
  include Datatype


  val index: 'a Value.t -> 'a -> Ty.t -> t
  (** Return the corresponding node from a value *)

  val node: t -> Node.t

  val ty: t -> Ty.t

end

(** {3 For building a particular value} *)

module type RegisteredValue = sig
  type s
  val key: s Value.t

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

val check_initialization: unit -> bool
(** Check if the initialization of all the sem and value have been done *)

(** {2 Only for the solver } *)
module Only_for_solver: sig
  type sem_of_node =
    | Sem: 'a Sem.t * 'a -> sem_of_node

  val nodesem:
    Node.t -> NodeSem.t option
    (** give the sem associated with a node, make sense only for not merged
        class. So only the module solver can use it *)

  val sem_of_node:
    NodeSem.t -> sem_of_node
    (** give the sem associated with a node, make sense only for not merged
        class. So only the module solver can use it *)

  type value_of_node =
    | Value: 'a Value.t * 'a -> value_of_node

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
    | Fresh_to_reg: ('event,'r) Dem.t * 'event -> opened_node
    | Sem  : NodeSem.t -> opened_node
    | Value  : NodeValue.t -> opened_node

  val open_node: Node.t -> opened_node

end
