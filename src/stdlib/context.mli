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

(** Context and backtrack point management *)

type context
(** A context, with an history of backtrack point *)

type bp
(** A backtrack point associated to a context *)

val create: unit -> context
(** Create a new context, with a base backtrack point.
    It is not possible to go below this backtrack point.
*)

val bp: context -> bp
(** Get the current backtrack point *)

val push : context -> unit
(** Push a new backtrack point *)


exception AlreadyPoped

val pop : bp -> unit
(** Pop the context associated to this backtrack point to this
    backtrack point. All the backtrack point created since the given backtrack point are also poped.

    raise AlreadyPoped if it already has been poped.
 *)

module Ref: sig
  type 'a t
  (** A reference aware of a context *)

  val create: context -> 'a -> 'a t
  (** Create a reference in this context with the given value *)

  val set: 'a t -> 'a -> unit
  (** Modify the reference *)

  val get: 'a t -> 'a
  (** Get the current value of the reference *)
end

type 'a history
(** history of the values *)

module Make(S:sig
    type t
    (** a type to make context aware *)

    type saved
    (** The data to save at backtrack point *)

    val save: t -> saved
    (** Get the data to save from the original type *)

    val restore: saved -> t -> unit
    (** Restore the saved data after a pop (delayed at the next {!refresh}) *)

    val get_history: t -> saved history
  end): sig

  val create: context -> S.saved history
  (** Create an history *)

  val refresh: S.t -> unit
  (** Function to call before accessing the value when a pop could have occured *)

  val save: S.t -> unit
  (** Function to call before modifying the value, it does also refresh *)

  type hidden
  (** To be used for enforcing the use of the previous function *)
  val ro: hidden -> S.t
  val rw: hidden -> S.t
  val hide: S.t -> hidden
end
