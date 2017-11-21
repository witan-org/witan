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

(** {2 Types} *)

module Ty : sig
  module Constr: Strings.Fresh
  (** type constructors *)

  type ty = private { ctr: Constr.t; args: ty IArray.t; tag: int}

  include Datatype with type t = ty
  (** types *)

  val app: Constr.t -> ty IArray.t -> ty
  val ctr: Constr.t -> ty

end


(** the key shouldn't be used before its registration and shouldn't be
    registered again *)
exception UnregisteredKey
exception AlreadyRegisteredKey

module Sem: Keys.Key
module Value: Keys.Key
module Dom: Keys.Key

type 'a dom = 'a Dom.t

type 'a sem = 'a Sem.t

module type Sem = sig
  include Datatype

  val key: t sem
end

val get_sem: 'a sem -> (module Sem with type t = 'a)
val sem_uninitialized: 'a sem -> bool
val print_sem : 'a sem -> 'a Pp.pp

type 'a value = 'a Value.t

module type Value = sig
  include Datatype

  val key: t value
end

val get_value: 'a value -> (module Value with type t = 'a)
val value_uninitialized: 'a value -> bool
val print_value : 'a value -> 'a Pp.pp


module Env: Keys.Key
type 'a env = 'a Env.t

module Dem: Keys.Key2
type ('k,'d) dem = ('k,'d) Dem.t

(** Classes *)
module Cl : sig
  include Datatype

  val fresh: ?to_reg:(('event,'r) dem * 'event) -> string -> Ty.t -> t
  (** the string is used as the prefix for the debug output *)

  val rename: t -> string -> unit
  (** to use with care *)

  val ty: t -> Ty.t

  val index: 'a sem -> 'a -> Ty.t -> t
  (** Return the corresponding cl from a semantical term *)
end

module ClSem: sig
  include Datatype


  val index: 'a sem -> 'a -> Ty.t -> t
  (** Return the corresponding cl from a semantical term *)

  val cl: t -> Cl.t

  val ty: t -> Ty.t

end

module type RegisteredSem = sig
  type s
  val key: s sem

  (** clsem *)
  include Datatype

  val index: s -> Ty.t -> t
  (** Return a clsem from a semantical term *)

  val cl: t -> Cl.t
  (** Return a class from a clsem *)

  val ty: t -> Ty.t
  (** Return the type from a clsem *)

  val sem: t -> s
  (** Return the sem from a clsem *)

  val clsem: t -> ClSem.t
  val of_clsem: ClSem.t -> t option

  val coerce_clsem: ClSem.t -> t

end


module RegisterSem (D:Sem) : RegisteredSem with type s = D.t


module ClValue: sig
  include Datatype


  val index: 'a value -> 'a -> Ty.t -> t
  (** Return the corresponding cl from a value *)

  val cl: t -> Cl.t

  val ty: t -> Ty.t

end

module type RegisteredValue = sig
  type s
  val key: s value

  (** clvalue *)
  include Datatype

  val index: s -> Ty.t -> t
  (** Return a clvalue from a valueantical term *)

  val cl: t -> Cl.t
  (** Return a class from a clvalue *)

  val ty: t -> Ty.t
  (** Return the type from a clvalue *)

  val value: t -> s
  (** Return the value from a clvalue *)

  val clvalue: t -> ClValue.t
  val of_clvalue: ClValue.t -> t option

  val coerce_clvalue: ClValue.t -> t

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
  type sem_of_cl =
    | Sem: 'a sem * 'a -> sem_of_cl

  val clsem:
    Cl.t -> ClSem.t option
    (** give the sem associated with a cl, make sense only for not merged
        class. So only the module solver can use it *)

  val sem_of_cl:
    ClSem.t -> sem_of_cl
    (** give the sem associated with a cl, make sense only for not merged
        class. So only the module solver can use it *)

  type value_of_cl =
    | Value: 'a value * 'a -> value_of_cl

  val clvalue:
    Cl.t -> ClValue.t option
    (** give the value associated with a cl, make sense only for not merged
        class. So only the module solver can use it *)

  val value_of_cl:
    ClValue.t -> value_of_cl
    (** give the value associated with a cl, make sense only for not merged
        class. So only the module solver can use it *)

  val cl_of_clsem: ClSem.t -> Cl.t
  val cl_of_clvalue: ClValue.t -> Cl.t

  type opened_cl =
    | Fresh: opened_cl
    | Fresh_to_reg: ('event,'r) dem * 'event -> opened_cl
    | Sem  : ClSem.t -> opened_cl
    | Value  : ClValue.t -> opened_cl

  val open_cl: Cl.t -> opened_cl

end
