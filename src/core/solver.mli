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

open Explanation
open Typedef

exception NotNormalized

type exp_same_sem =
| ExpSameSem   : pexp * Cl.t * ClSem.t -> exp_same_sem
| ExpSameValue : pexp * Cl.t * ClValue.t -> exp_same_sem

val exp_same_sem : exp_same_sem Explanation.exp

exception UninitializedEnv of Env.K.t

module type Ro = sig
  type t
  (** {3 Immediate information} *)
  val register : t -> Cl.t -> unit
  (** Add a new class to register *)

  val is_equal      : t -> Cl.t -> Cl.t -> bool
  val find_def  : t -> Cl.t -> Cl.t
  val get_dom   : t -> 'a dom -> Cl.t -> 'a option
    (** dom of the class *)
  val get_value   : t -> 'a value -> Cl.t -> 'a option
    (** value of the class *)

  (** {4 The classes must have been marked has registered} *)

  val find      : t -> Cl.t -> Cl.t
  val is_repr      : t -> Cl.t -> bool

  val is_registered : t -> Cl.t -> bool

  val get_env : t -> 'a env -> 'a
  val set_env: t -> 'a env -> 'a -> unit

  val is_current_env: t -> bool

end

module Ro : Ro

module Delayed : sig
  type t = private Ro.t
  include Ro with type t := t

  (** {3 Immediate modifications} *)
  val set_dom  : t -> pexp -> 'a dom -> Cl.t -> 'a -> unit
    (** change the dom of the equivalence class *)

  val set_sem  : t -> Explanation.pexp -> Cl.t -> ClSem.t -> unit
  (** attach a sem to an equivalence class *)

  val set_clvalue: t -> Explanation.pexp -> Cl.t -> ClValue.t -> unit
  (** attach value to an equivalence class *)

  val set_value: t -> Explanation.pexp -> 'a value -> Cl.t -> 'a -> unit
  (** attach value to an equivalence class *)

  val set_dom_premerge  : t -> 'a dom -> Cl.t -> 'a -> unit
    (** [set_dom_premerge d cl] must be used only during the merge of two class
        [cl1] and [cl2], with one of them being [cl].
        The explication is the explication given for the merge
    *)

  val unset_dom  : t -> pexp -> 'a dom -> Cl.t -> unit
  (** remove the dom of the equivalence class *)

  (** {3 Delayed modifications} *)
  val merge    : t -> Explanation.pexp -> Cl.t -> Cl.t -> unit

  (** {3 Attach Event} *)
  val attach_dom: t -> Cl.t -> 'a dom -> ('event,'r) dem -> 'event -> unit
    (** wakeup when the dom change *)
  val attach_value: t -> Cl.t -> 'a value -> ('event,'r) dem -> 'event -> unit
    (** wakeup when a value is attached to this equivalence class *)
  val attach_reg_cl: t -> Cl.t -> ('event,'r) dem -> 'event -> unit
    (** wakeup when this cl is registered *)
  val attach_reg_sem: t -> 'a sem -> ('event,'r) dem -> 'event -> unit
    (** wakeup when a new semantical class is registered *)
  val attach_cl: t -> Cl.t -> ('event,'r) dem -> 'event -> unit
    (** wakeup when it is not anymore the representative class *)

  (** other event can be added *)

  val register_decision: t -> Explanation.chogen -> unit
  (** register a decision that would be scheduled later. The
      [make_decision] of the [Cho] will be called at that time to know
      if the decision is still needed. *)
  val mk_pexp: t -> ?age:age -> ?tags:tags -> 'a exp -> 'a -> Explanation.pexp
  val current_age: t -> age
  val contradiction: t -> Explanation.pexp -> 'b

  val flush: t -> unit
(** Apply all the modifications and direct consequences.
    Should be used only during wakeup of not immediate daemon
*)
end

type d = Delayed.t

module Wait : Events.Wait.S with type delayed = Delayed.t and type delayed_ro = Ro.t

(** {2 Domains and Semantic Values key creation} *)

module type Dom = sig
  type t

  val merged: t option -> t option -> bool
    (** Check if two values of the domain are merged (equal) *)

  val merge:
    Delayed.t -> pexp ->
    t option * Cl.t (* cl1 *) ->
    t option * Cl.t (* cl2 *) ->
    (** Never with both None *)
    bool (** true: cl1 will be repr otherwise it is cl2 *) ->
    unit

  val pp: Format.formatter  -> t  -> unit
  val key: t dom


end

module RegisterDom (D:Dom) : sig end


val register_env: 'a Pp.pp -> 'a env -> unit
val print_env: 'a env -> 'a Pp.pp

(** {2 External use of the solver} *)
type t

val new_t    : unit -> t

val new_delayed :
  sched_daemon:(Events.Wait.daemon_key -> unit) ->
  sched_decision:(chogen -> unit) ->
  t -> Delayed.t
(** The solver shouldn't be used anymore before
    calling flush. (flushd doesn't count)
*)

exception Contradiction of Explanation.pexp

val run_daemon: Delayed.t -> Events.Wait.daemon_key -> unit
(** schedule the run of a deamon *)

val delayed_stop: Delayed.t -> unit
(** Apply all the modifications and direct consequences.
    The argument shouldn't be used anymore *)

val flush: Delayed.t -> unit
(** Apply all the modifications and direct consequences.
    The argument can be used after that *)


(*
val make_decisions : Delayed.t -> attached_daemons -> unit
*)

val get_dom   : t -> 'a dom -> Cl.t -> 'a option
    (** dom of the representative class *)
val get_value : t -> 'a value -> Cl.t -> 'a option
    (** value of the representative class *)

val find      : t -> Cl.t -> Cl.t
val is_equal  : t -> Cl.t -> Cl.t -> bool

val get_trail : t -> Explanation.t
val new_dec : t -> Explanation.dec
val current_age : t -> Explanation.Age.t
val current_nbdec : t -> int

(** for conflict *)
val get_direct_dom   : t -> 'a dom -> Cl.t -> 'a option
    (** dom of the class directly (the last time modified) *)

(** {2 Implementation Specifics } *)
(** Because this module is implemented with persistent datastructure *)

val new_handler: t -> t
(** Modification in one of the environnement doesn't modify the other *)

(** Debug *)
val draw_graph: ?force:bool -> t -> unit
val output_graph : string -> t -> unit

val check_initialization: unit -> bool
(** Check if the initialization of all the dom, sem and dem have been done *)

val print_dom: 'a dom -> 'a Pp.pp
val print_dom_opt: 'a dom -> 'a option Pp.pp
