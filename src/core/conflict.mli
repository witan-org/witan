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
open Typedef

(** Decision, Conflict and Learning *)

val print_decision: Debug.flag

(** {2 Decision} *)
module Cho = Trail.Cho

type 'd decdone  =
| DecNo (** No decision to do *)
| DecTodo of 'd (** This decision should be done *)

module type Cho = sig
  (** Allows to keep any information for the potential decision *)
  module OnWhat  : Stdlib.Datatype

  val choose_decision:
    Egraph.Delayed.t -> OnWhat.t -> (Egraph.Delayed.t -> unit) decdone
  (** Answer the question: Is the decision still needed? *)

  val key: OnWhat.t Cho.t

end

val register_cho: (module Cho with type OnWhat.t = 'a) -> unit

val choose_decision: Egraph.Delayed.t -> Trail.chogen -> (Egraph.Delayed.t -> unit) decdone

module ChoGenH : Stdlib.XHashtbl.S with type key = Trail.chogen

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

  val apply_learnt: t list -> Typedef.Node.t list
  (** Build the constraint that correspond to the conflict learnt *)

  val levels: Conflict.t -> t -> Levels.t
  (** iterate on what depends the conflict (classe and value). *)

  val useful_nodes: t -> Node.t Bag.t
  (** used at the end to know which node are useful for decision heuristics *)
end

val register_con: (module Con) -> unit

(** {2 Conflict analysis} *)

module Learnt: Stdlib.Datatype

val learn: Trail.t -> Trail.Pexp.t -> Trail.Age.t * Learnt.t * Node.t Bag.t
(** Return the backtracking age, the constraint learnt and the useful nodes *)

val apply_learnt: Egraph.Delayed.t -> Learnt.t -> unit


(** {2 Generic conflict} *)

module EqCon : sig

  type t = {
    l: Node.t;
    r: Node.t;
  }

  val pp: t Witan_popop_lib.Pp.pp

  val key : t Con.t

  val register_apply_learnt: Ty.t -> (t list -> Node.t list) -> unit

  val split: Conflict.t -> t -> Node.t -> Node.t -> t list
end

(** {2 From boolean theory } *)
val _or: (Node.t list -> Node.t) ref
val _set_true: (Egraph.Delayed.t -> Trail.Pexp.t -> Node.t -> unit) ref