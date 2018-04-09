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

(** Witan Typed terms

    This module defines higher-order typed terms. These terms are designed
    to faithfully represent input terms, as well as proof terms. *)

(** {2 Terms} *)

type binder = private
  | Pi      (** Dependant product *)
  | Arrow   (** Function type *)
  | Lambda  (** Function binder *)
  | Forall  (** Universal quantification *)
  | Exists  (** Existencial quantification *)
(** Available binders *)

type id = t Id.t
(** Identifiers in terms. Used for bound/free variables, constants, etc... *)

and descr = private
  | Type
  (** The Universe at the root of everything *)
  | Id of id
  (** Identifiers (i.e variables, constants) *)
  | App of t * t
  (** Curried application *)
  | Let of id * t * t
  (** Local let binding, as (var, expr, body). *)
  | Binder of binder * id * t
  (** Variable binding, without argument/value *)
(** Term descriptors. *)

and t = private {
  ty : t;
  term : descr;
  mutable hash : int;
}
(** Term records. Contains the type of the term,
    to avoid recomputing it every time (which is pretty
    often in the context of proof checking). *)

exception Function_expected of t
(** Exception raised when applying a non-function to an argument.
    The given term is the one that's expected to be a function. *)

exception Type_mismatch of t * t
(** Exception raised during typechecking of application, with a
    pair [(arg, ty)] where [arg] is the provided argument and
    [ty] the expected type of the argument. *)


(** {2 Term ids} *)

module Id : Stdlib.Datatype with type t = id
(** Adequate module defining term identifiers for functor instanciation such
    as Set/Map/etc... *)


(** {2 Term inspection} *)

val hash : t -> int
(** standard hash function *)

val equal : t -> t -> bool
(** standard equality function *)

val compare: t -> t -> int
(** standard comparison function *)

val print : Format.formatter -> t -> unit
val pp: t Pp.pp
(** Print a term (quite verbose). *)

include Stdlib.Datatype with type t := t

(** {2 Term creation} *)

val _Type : t
(** The term at the root of everything. *)

val _Prop : t
val _Prop_id : id
(** The term for the type of propositions. *)

val const : id -> t
(** Create a term from an identifier. *)

val app : t -> t -> t
val apply : t -> t list -> t
(** Term application (curried and uncurried). *)

val letin : id -> t -> t -> t
(** Local let, as [letin v e body],binds [v] to [e] in [body]. *)

val pi : id -> t -> t
val pis : id list -> t -> t
(** Dependant product. *)

val lambda : id -> t -> t
val lambdas : id list -> t -> t
(** Function construction. *)

val arrow : t -> t -> t
val arrows : t list -> t -> t
(** Function type creation. *)

val forall : id -> t -> t
val foralls : id list -> t -> t
(** Universal quantification. *)

val exist : id -> t -> t
val exists : id list -> t -> t
(** Existencial quantification. *)

(** {2 Term constants} *)

val true_id : id
val true_term : t
(** [true] constant *)

val false_id : id
val false_term : t
(** [false] constant *)

val equal_id : id
val equal_term : t
(** equality *)

val distinct_id : int -> id
val distinct_term : int -> t
val is_distinct_id : id -> bool
val is_distinct_term : t -> bool
(** one distinct by size arity *)

val not_id : id
val not_term : t
(** Propositional negation *)

val imply_id : id
val imply_term : t
(** Propostional implication *)

val equiv_id : id
val equiv_term : t
(** Propositional equivalence *)

val or_id : int -> id
val or_term : int -> t
val is_or_id : id -> bool
val is_or_term : t -> bool
(** Propositional disjunction *)

val and_id : int -> id
val and_term : int -> t
val is_and_id : id -> bool
val is_and_term : t -> bool
(** Propositional conjunction *)


val ite_id : id
val ite_term : t
(** If-then-else *)

val is_defined: id -> bool

(** {2 Arithmetic } *)
val _Real : t

val const_real_id: string -> id
val is_const_real_id: id -> bool
val const_real_term: string -> t
val is_const_real_term: t -> bool
val get_const_real_id: id -> Q.t
val get_const_real_term: t -> Q.t

val add_real_id : int -> id
val add_real_term : int -> t
val is_add_real_id : id -> bool
val is_add_real_term : t -> bool

val lt_real_id : int -> id
val lt_real_term : int -> t
val is_lt_real_id : id -> bool
val is_lt_real_term : t -> bool

val le_real_id : int -> id
val le_real_term : int -> t
val is_le_real_id : id -> bool
val is_le_real_term : t -> bool

val sub_real_id : id
val neg_real_id : id
val mul_real_id : id
val div_real_id : id

val sub_real_term : t
val neg_real_term : t
val mul_real_term : t
val div_real_term : t

(** {2 Term substitution} *)

module Subst : Map.S with type key = id
(** Substitutions, aka mapping from variables to terms. *)

val subst : t Subst.t -> t -> t
(** Substitution on terms. Correctly handles *)


(** {2 Term destruction} *)

val uncurry_app : t -> t * t list
(** Uncurry the application. *)

val uncurry : ?assoc:(id -> [`Left | `Right] option) -> t -> t * t list
(** Uncurry a term, using the associtivity information
    in the tag. *)

val uncurry_assoc_left : id -> t list -> t list
val uncurry_assoc_right : id -> t list -> t list
(** Uncurry a left (or right) associative symbol in a term. *)

val flatten_binder : binder -> t -> id list * t
(** Get the list of all consecutive variables bound by the same binder in a term. *)

val concat_vars : id list -> (t * id list) list
(** Groups variables by types. *)

val extract_fun_ty : t -> Id.t * t
