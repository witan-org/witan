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

(** Node *)

module Node : sig
  include Datatype

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
val check_sem_registered: 'a Sem.t -> unit
val print_sem : 'a Sem.t -> 'a Pp.pp

module ThTerm: sig
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

  (** thterm *)
  include Datatype

  val index: s -> Ty.t -> t
  (** Return a semantical term from the user type *)

  val node: t -> Node.t
  (** Return a class from a thterm *)

  val ty: t -> Ty.t
  (** Return the type from a thterm *)

  val sem: t -> s
  (** Return the sem from a thterm *)

  val thterm: t -> ThTerm.t
  val of_thterm: ThTerm.t -> t option
  (** Return the user type if the ThTerm belongs to this module *)

  val coerce_thterm: ThTerm.t -> t
  (** Return the user type. Raise if the ThTerm does not belong to this
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
val check_value_registered: 'a Value.t -> unit

(** {3 Module for handling generically values} *)

module Values: sig
  include Datatype


  val index: 'a Value.t -> 'a -> Ty.t -> t
  (** Return the corresponding node from a value *)

  val node: t -> Node.t

  val ty: t -> Ty.t

  val value: 'a Value.t -> t -> 'a option
end

(** {3 For building a particular value} *)

module type RegisteredValue = sig
  type s
  val key: s Value.t

  (** nodevalue *)
  include Datatype

  val index: ?basename:string -> s -> Ty.t -> t
  (** Return a nodevalue from a valueantical term.
      Basename is used only for debug
  *)

  val node: t -> Node.t
  (** Return a class from a nodevalue *)

  val ty: t -> Ty.t
  (** Return the type from a nodevalue *)

  val value: t -> s
  (** Return the value from a nodevalue *)

  val nodevalue: t -> Values.t
  val of_nodevalue: Values.t -> t option

  val coerce_nodevalue: Values.t -> t

end

module RegisterValue (D:Value) : RegisteredValue with type s = D.t

val check_initialization: unit -> bool
(** Check if the initialization of all the sem and value have been done *)

(** {2 Only for the solver } *)
module Only_for_solver: sig
  type sem_of_node =
    | Sem: 'a Sem.t * 'a -> sem_of_node

  val thterm:
    Node.t -> ThTerm.t option
    (** give the sem associated with a node, make sense only for not merged
        class. So only the module solver can use it *)

  val sem_of_node:
    ThTerm.t -> sem_of_node
    (** give the sem associated with a node, make sense only for not merged
        class. So only the module solver can use it *)

  type value_of_node =
    | Value: 'a Value.t * 'a -> value_of_node

  val nodevalue:
    Node.t -> Values.t option
    (** give the value associated with a node, make sense only for not merged
        class. So only the module solver can use it *)

  val value_of_node:
    Values.t -> value_of_node
    (** give the value associated with a node, make sense only for not merged
        class. So only the module solver can use it *)

  val node_of_thterm: ThTerm.t -> Node.t
  val node_of_nodevalue: Values.t -> Node.t

  type opened_node =
    | Sem  : ThTerm.t -> opened_node
    | Value  : Values.t -> opened_node

  val open_node: Node.t -> opened_node

end
