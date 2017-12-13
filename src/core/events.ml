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

let debug = Debug.register_info_flag ~desc:"for the events" "Egraph.events"

(** Dem *)

module Dem = Keys.Make_key2(struct end)

module Print = struct (** Cutting the knot for pp *)

  type pdem_event = { mutable
      pdem_event : 'k 'd. ('k,'d) Dem.t -> 'k Pp.pp}

  let pdem_event : pdem_event =
    {pdem_event = fun _ _ _ -> assert false} (** called too early *)
  let dem_event dem fmt s = pdem_event.pdem_event dem fmt s

  type pdem_runable = { mutable
      pdem_runable : 'k 'd. ('k,'d) Dem.t -> 'd Pp.pp}

  let pdem_runable : pdem_runable =
    {pdem_runable = fun _ _ _ -> assert false} (** called too early *)
  let dem_runable dem fmt s = pdem_runable.pdem_runable dem fmt s


end


module Fired = struct
  type 'b event =
    (** the domain dom of the class change *)
    | EventDom    : Node.t * 'a Dom.t  *      'b -> 'b event
    (** the value of the class has been set *)
    | EventValue    : Node.t * 'a Value.t  *  'b -> 'b event
    (** a new semantical term 'a point to this class (not complete) *)
    | EventSem    : Node.t * 'a Sem.t  * 'a * 'b -> 'b event
    (** we want to register a class *)
    | EventReg    : Node.t *                'b -> 'b event
    (** we want to register this class *)
    | EventRegNode  : Node.t *                'b -> 'b event
    (** This class is not the representant of its eq-class anymore *)
    | EventChange : Node.t *                'b -> 'b event
    (** a new semantical term 'a appear *)
    | EventRegSem : NodeSem.t * 'b -> 'b event
    (** a new value 'a appear *)
    | EventRegValue : NodeValue.t * 'b -> 'b event

  let pp fmt = function
    | EventDom      (node, dom, _) ->
      Format.fprintf fmt "dom:%a of %a" Dom.pp dom Node.pp node
    | EventValue    (node, value, _) ->
      Format.fprintf fmt "value:%a of %a" Value.pp value Node.pp node
    | EventSem      (node, sem, v, _) ->
      Format.fprintf fmt "sem:%a of %a with %a"
        Sem.pp sem Node.pp node (print_sem sem) v
    | EventReg      (node, _)    ->
      Format.fprintf fmt "any registration of %a" Node.pp node
    | EventRegNode    (node, _)    ->
      Format.fprintf fmt "registration of %a" Node.pp node
    | EventChange   (node, _)    ->
      Format.fprintf fmt "change of %a" Node.pp node
    | EventRegSem (nodesem, _) ->
      let node = Only_for_solver.node_of_nodesem nodesem in
      begin match Only_for_solver.sem_of_node nodesem with
        | Only_for_solver.Sem(sem,v) ->
          Format.fprintf fmt "registration of sem:%a of %a with %a"
            Sem.pp sem Node.pp node (print_sem sem) v
      end
    | EventRegValue (nodevalue, _) ->
      let node = Only_for_solver.node_of_nodevalue nodevalue in
      begin match Only_for_solver.value_of_node nodevalue with
        | Only_for_solver.Value(value,v) ->
          Format.fprintf fmt "registration of value:%a of %a with %a"
            Value.pp value Node.pp node (print_value value) v
      end

  let get_data = function
    | EventDom      (_, _ , d)   -> d
    | EventValue    (_, _ , d)   -> d
    | EventSem      (_, _, _, d) -> d
    | EventReg    (_, d)       -> d
    | EventRegNode  (_, d)       -> d
    | EventChange   (_, d)       -> d
    | EventRegSem (_, d) -> d
    | EventRegValue (_,d) -> d

  type 'b t = 'b event list
end

module Wait = struct
  type t =
    | Event: ('k,'d) Dem.t * 'k -> t

  let pp fmt = function
    | Event (dem, event) ->
      let f (type k) (type d) (dem:(k,d) Dem.t) (event : k) =
        Format.fprintf fmt "Demon %a event %a"
          Dem.pp dem (Print.dem_event  dem) event
      in
      f dem event

  type _ enqueue =
    | EnqRun: 'r -> 'r enqueue
    | EnqAlready: _ enqueue
    | EnqRedirected: ('e,'r) Dem.t * 'e -> _ enqueue
    | EnqStopped: _ enqueue

  type daemon_key =
    | DaemonKey: ('k,'runable) Dem.t * 'runable -> daemon_key


  type 'a translate = { translate : 'd. 'a -> 'd -> 'd Fired.event}

  let translate_dom =
    {translate = fun (node,dom) data -> EventDom(node,dom,data)}
  let translate_value =
    {translate = fun (node,value) data -> EventValue(node,value,data)}
  let translate_sem =
    {translate = fun (node,sem,s) data -> EventSem(node,sem,s,data)}
  let translate_reg =
    {translate = fun node data -> EventReg(node,data)}
  let translate_regnode =
    {translate = fun node data -> EventRegNode(node,data)}
  let translate_change =
    {translate = fun node data -> EventChange(node,data)}
  let translate_regsem =
    {translate = fun nodesem data -> EventRegSem(nodesem,data)}
  let translate_regvalue =
    {translate = fun nodeval data -> EventRegValue(nodeval,data)}


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
    end) : S with type delayed = S.delayed
              and type delayed_ro = S.delayed_ro = struct

    type delayed = S.delayed
    type delayed_ro = S.delayed_ro

    module type Dem = sig
      type runable
      val print_runable: runable Pp.pp
      val run: delayed -> runable -> runable option

      type event
      val print_event: event Pp.pp
      val enqueue: delayed_ro -> event Fired.event -> runable enqueue

      val key: (event,runable) Dem.t
      val immediate: bool

    end

    module Registry = Dem.Make_Registry(struct
        type ('k,'d) data = (module Dem with type event = 'k and type runable = 'd)
        let ppk (type k) (type d) (dem: (k,d) data) =
          let module Dem = (val dem) in
          Dem.print_event
        let ppd (type k) (type d) (dem: (k,d) data) =
          let module Dem = (val dem) in
          Dem.print_runable
        let key (type k) (type d) (dem: (k,d) data) =
          let module Dem = (val dem) in
          Dem.key
      end
      )

    let register_dem = Registry.register
    let get_dem = Registry.get

    let print_dem_event = Registry.printk
    let () = Print.pdem_event.Print.pdem_event <- print_dem_event

    let print_dem_runable = Registry.printd
    let () = Print.pdem_runable.Print.pdem_runable <- print_dem_runable

    let is_well_initialized = Registry.is_well_initialized

    let new_pending_daemon (type k) (type d) t (dem:(k,d) Dem.t) runable =
      let module Dem = (val get_dem dem) in
      let daemonkey = DaemonKey(dem, runable) in
      if Dem.immediate
      then S.schedule_immediate t daemonkey
      else S.schedule t daemonkey

    let wakeup_event translate t info wevent =
      match wevent with
      | Event (dem,event) ->
        let rec f : type event r. S.delayed -> (event,r) Dem.t -> event -> unit =
          fun t dem event ->
            let module Dem = (val get_dem dem) in
            let event = translate.translate info event in
            match Dem.enqueue (S.readonly t) event with
            | EnqStopped -> () (** todo remove from the list of event *)
            | EnqAlready -> ()
            | EnqRedirected(dem,event) -> f t dem event
            | EnqRun runable -> new_pending_daemon t dem runable
        in
        f t dem event

    let wakeup_events_list translate t events info =
      match events with
      | None | Some [] ->
        Debug.dprintf0 debug "[Egraph] @[No scheduling@]"
      | Some events ->
        List.iter (wakeup_event translate t info) events

    let wakeup_events_bag translate t events info =
      let is_empty = match events with
        | None -> true
        | Some events -> Bag.is_empty events in
      if is_empty then Debug.dprintf0 debug "[Egraph] @[No scheduling@]"
      else Bag.iter (wakeup_event translate t info) (Opt.get events)


  end
end
