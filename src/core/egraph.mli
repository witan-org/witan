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

(** Egraph is the main module of core *)

(** The solver contains all the information. It keeps track of
    equivalence classes, values. It take care to schedule event that
    happened. *)

open Trail
open Nodes

exception NotRegistered

exception UninitializedEnv of Env.K.t

exception Contradiction of Trail.Pexp.t

module type Getter = sig
  type t

  val is_equal  : t -> Node.t -> Node.t -> bool
  val find_def  : t -> Node.t -> Node.t
  val get_dom   : t -> 'a Dom.t -> Node.t -> 'a option
    (** dom of the class *)
  val get_value : t -> 'a ValueKind.t -> Node.t -> 'a option
    (** value of the class *)

  (** {4 The classes must have been registered} *)

  val find      : t -> Node.t -> Node.t
  val is_repr   : t -> Node.t -> bool

  val is_registered : t -> Node.t -> bool

  val get_env : t -> 'a Env.t -> 'a
  val set_env : t -> 'a Env.t -> 'a -> unit

  val context : t -> Context.creator

end

module type Ro = sig
  type t
  include Getter with type t := t

  val register : t -> Node.t -> unit
  (** Add a new node to register *)

  val is_current_env: t -> bool

end

module Ro : Ro

type t = private Ro.t
include Ro with type t := t

(** {3 Immediate modifications} *)
val set_dom  : t -> 'a Dom.t -> Node.t -> 'a -> unit
(** change the dom of the equivalence class *)

val unset_dom  : t -> 'a Dom.t -> Node.t -> unit
(** remove the dom of the equivalence class *)


(** {3 Delayed modifications} *)
val set_thterm  : t -> Trail.Pexp.t -> Node.t -> ThTerm.t -> unit
(** attach a theory term to an equivalence class *)

val set_value: t -> Trail.Pexp.t -> Node.t -> Value.t -> unit
(** attach value to an equivalence class *)

val merge    : t -> Trail.Pexp.t -> Node.t -> Node.t -> unit


(** {3 Attach Event} *)
val attach_dom: t -> Node.t -> 'a Dom.t -> ('event,'r) Events.Dem.t -> 'event -> unit
(** wakeup when the dom change *)
val attach_value: t -> Node.t -> 'a ValueKind.t -> ('event,'r) Events.Dem.t -> 'event -> unit
(** wakeup when a value is attached to this equivalence class *)
val attach_any_value: t -> Node.t -> ('event,'r) Events.Dem.t -> 'event -> unit
(** wakeup when any kind of value is attached to this equivalence class *)
val attach_reg_node: t -> Node.t -> ('event,'r) Events.Dem.t -> 'event -> unit
(** wakeup when this node is registered *)
val attach_reg_sem: t -> 'a ThTermKind.t -> ('event,'r) Events.Dem.t -> 'event -> unit
(** wakeup when a new semantical class is registered *)
val attach_reg_value: t -> 'a ValueKind.t -> ('event,'r) Events.Dem.t -> 'event -> unit
(** wakeup when a new value is registered *)
val attach_node: t -> Node.t -> ('event,'r) Events.Dem.t -> 'event -> unit
(** wakeup when it is not anymore the representative class *)

val register_decision: t -> Trail.chogen -> unit
(** register a decision that would be scheduled later. The
    [choose_decision] of the [Cho] will be called at that time to know
    if the decision is still needed. *)

(** {3 Trails} *)
val mk_pexp: t -> ?age:age -> 'a Exp.t -> 'a -> Trail.Pexp.t
val current_age: t -> age
val add_pexp: t -> Trail.Pexp.t -> unit
val contradiction: t -> Trail.Pexp.t -> 'b

(** {3 Low level} *)
val flush_internal: t -> unit
(** Apply all the modifications and direct consequences.
    Should be used only during wakeup of not immediate daemon
*)

module Wait : Events.Wait.S with type delayed = t and type delayed_ro = Ro.t


(** {2 Domains and Semantic Values key creation} *)

module type Dom = Dom.Dom_partial with type delayed := t and type pexp := Trail.Pexp.t

val register_dom : (module Dom with type t = 'a) -> unit

val check_initialization: unit -> bool
(** Check if the initialization of all the dom, sem and dem have been done *)

val print_dom: 'a Dom.t -> 'a Format.printer
val print_dom_opt: 'a Dom.t -> 'a option Format.printer

module Getter : Getter

(** {2 External use of the solver} *)
module Backtrackable: sig
  type delayed = t
  include Getter

  val new_t    : Context.creator -> t

  val new_delayed :
    sched_daemon:(Events.Wait.daemon_key -> unit) ->
    sched_decision:(chogen -> unit) ->
    t -> delayed
  (** The solver shouldn't be used anymore before
      calling flush. (flushd doesn't count)
  *)

  val run_daemon: delayed -> Events.Wait.daemon_key -> unit
  (** schedule the run of a deamon *)

  val delayed_stop: delayed -> unit
  (** Apply all the modifications and direct consequences.
      The argument shouldn't be used anymore *)

  val flush: delayed -> unit
  (** Apply all the modifications and direct consequences.
      The argument can be used after that *)



  (* val make_decisions : delayed -> attached_daemons -> unit *)

  val get_trail : t -> Trail.t
  val get_getter : t -> Getter.t
  val new_dec : t -> Trail.dec
  val current_age : t -> Trail.Age.t
  val current_nbdec : t -> int

  (** Debug *)
  val draw_graph: ?force:bool -> t -> unit
  val output_graph : string -> t -> unit
end
