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

open Typedef

module Fired : sig
  type 'b event =
    (** the domain dom of the class change *)
    | EventDom    : Node.t * 'a Dom.t  *      'b -> 'b event
    (** the value of the node has been set *)
    | EventValue    : Node.t * 'a Value.t  *  'b -> 'b event
    (** a new semantical term 'a point to this class (not complete) *)
    | EventSem    : Node.t * 'a Sem.t  * 'a * 'b -> 'b event
    (** we want to register a class *)
    | EventReg  : Node.t *                  'b -> 'b event
    (** we want to register this class *)
    | EventRegNode: Node.t *                  'b -> 'b event
    (** This class is not the representant of its eq-class anymore *)
    | EventChange : Node.t *                'b -> 'b event
    (** a new semantical term 'a appear *)
    | EventRegSem : NodeSem.t * 'b -> 'b event
    (** a new value 'a appear *)
    | EventRegValue : NodeValue.t * 'b -> 'b event

  val pp: 'b event Pp.pp
  val get_data: 'b event -> 'b

  type 'b t = 'b event list

end

module Wait : sig
  type t =
    | Event: ('k,'d) Dem.t * 'k -> t


  type _ enqueue =
    | EnqRun: 'r -> 'r enqueue
    | EnqAlready: _ enqueue
    | EnqRedirected: ('e,'r) Dem.t * 'e -> _ enqueue
    | EnqStopped: _ enqueue

  type daemon_key =
    | DaemonKey: ('k,'runable) Dem.t * 'runable -> daemon_key

  val pp: t Pp.pp

  type 'a translate = { translate : 'd. 'a -> 'd -> 'd Fired.event}

  val translate_dom : (Node.t * 'a Dom.t) translate
  val translate_value : (Node.t * 'a Value.t) translate
  val translate_reg : Node.t translate
  val translate_regnode : Node.t translate
  val translate_change : Node.t translate
  val translate_regsem : NodeSem.t translate
  val translate_regvalue : NodeValue.t translate

  module type S = sig
    type delayed
    type delayed_ro

    module type Dem =
    sig
      type runable
      val print_runable : runable Pp.pp
      val run : delayed -> runable -> runable option
      type event
      val print_event : event Pp.pp
      val enqueue : delayed_ro -> event Fired.event -> runable enqueue
      val key : (event, runable) Dem.t
      val immediate : bool
    end

    val register_dem : (module Dem with type event = 'k and type runable = 'd) -> unit

    val get_dem : ('k, 'd) Dem.t -> (module Dem with type event = 'k and type runable = 'd)

    val print_dem_event : ('a, 'b) Dem.t -> 'a Pp.pp

    val print_dem_runable : ('a, 'b) Dem.t -> 'b Pp.pp

    val new_pending_daemon : delayed -> ('a, 'b) Dem.t -> 'b -> unit

    val wakeup_event : 'a translate -> delayed -> 'a -> t -> unit

    val wakeup_events_list :
      'a translate -> delayed -> t list option -> 'a -> unit

    val wakeup_events_bag :
      'a translate -> delayed -> t Bag.t option -> 'a -> unit

    val is_well_initialized : unit -> bool
  end


  module Make(S:sig
      type delayed
      val schedule_immediate: delayed -> daemon_key -> unit
      val schedule: delayed -> daemon_key -> unit

      type delayed_ro
      val readonly : delayed -> delayed_ro
    end) : S with type delayed = S.delayed and type delayed_ro = S.delayed_ro
end
