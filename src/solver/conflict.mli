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

open Witan_core_structures

(** Decision, Conflict and Learning *)

(** {2 Decision} *)
module Cho = Trail.Cho

type 'd decdone  =
| DecNo (** No decision to do *)
| DecTodo of 'd (** This decision should be done *)

module type Cho = sig
  (** Allows to keep any information for the potential decision *)
  module OnWhat  : sig
    type t
    val pp: t Pp.pp
  end

  (** Allows to transfer any information from {!choose_decision} to {!make_decision} *)
  module What: sig
    type t
    val pp: t Pp.pp
  end

  val choose_decision:
    Egraph.Delayed.t -> OnWhat.t -> What.t decdone
  (** Answer the question: Is the decision still needed? *)

  val make_decision:
    Egraph.Delayed.t -> Trail.Pexp.t -> OnWhat.t -> What.t -> unit
  (** Propagate the decision using {!Egraph.Delayed.t} *)

  val key: (OnWhat.t,What.t) Cho.t

end

(** {2 Conflict} *)

module Conflict : sig
  (** Environment used during conflict resolution *)
  type t

  val age_merge: t -> Node.t -> Node.t -> Trail.Age.t
  (** Give the age at which the given node merged *)

end

module Exp = Trail.Exp
module Con = Trail.Con

module type Exp = sig

  type t

  val pp: t Pp.pp

  val key: t Trail.Exp.t

  val from_contradiction:
    Conflict.t (* -> Trail.Age.t *) -> t -> Trail.Pcon.t list
    (** First step of the analysis done on the trail. *)

  val analyse  :
    Conflict.t (* -> Trail.Age.t *) -> t -> 'a Con.t -> 'a -> Trail.Pcon.t list
    (** One step of the analysis done on the trail. This function is
       called on the explanation that correspond to last_level of the
        conflict *)

end

val register_exp: (module Exp) -> unit

(** {2 Levels} *)

module Levels : sig

  type t

  val empty: t

  val add: Conflict.t -> Trail.age -> t -> t

end

(** {2 Learning} *)

(* type levels = {levels: 'a. Typedef.Node.t -> 'a Typedef.Value.t -> unit} *)

module type Con = sig

  type t

  val pp: t Pp.pp

  val key: t Trail.Con.t

  val apply_learnt: t -> Typedef.Node.t
  (** Build the constraint that correspond to the conflict learnt *)

  val levels: Conflict.t -> t -> Levels.t
  (** iterate on what depends the conflict (classe and value). *)

end

val register_con: (module Con) -> unit

(** {2 Conflict analysis} *)

val learn: Trail.t -> Trail.Pexp.t -> Trail.Age.t * Node.t


val _or: (Node.t list -> Node.t) ref
