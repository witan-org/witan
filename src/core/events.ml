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

open Typedef

let debug = Debug.register_info_flag ~desc:"for the events" "Solver.events"

module Fired = struct
  type 'b event =
    (** the domain dom of the class change *)
    | EventDom    : Node.t * 'a Dom.t  *      'b -> 'b event
    (** the value of the class has been set *)
    | EventValue    : Node.t * 'a value  *  'b -> 'b event
    (** a new semantical term 'a point to this class (not complete) *)
    | EventSem    : Node.t * 'a sem  * 'a * 'b -> 'b event
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
    | Event: ('k,'d) dem * 'k -> t

  let pp fmt = function
    | Event (dem, event) ->
      let f (type k) (type d) (dem:(k,d) dem) (event : k) =
        Format.fprintf fmt "Demon %a event %a"
          Dem.pp dem (Print.dem_event  dem) event
      in
      f dem event

  type _ enqueue =
    | EnqRun: 'r -> 'r enqueue
    | EnqAlready: _ enqueue
    | EnqRedirected: ('e,'r) dem * 'e -> _ enqueue
    | EnqStopped: _ enqueue

  type daemon_key =
    | DaemonKey: ('k,'runable) dem * 'runable -> daemon_key


  type 'a translate = { translate : 'd. 'a -> 'd -> 'd Fired.event}

  let translate_dom =
    {translate = fun (node,dom) data -> EventDom(node,dom,data)}
  let translate_value =
    {translate = fun (node,value) data -> EventValue(node,value,data)}
  (* let translate_sem = *)
  (*   {translate = fun (node,sem,s) data -> EventSem(node,sem,s,data)} *)
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
      val key : (event, runable) dem
      val immediate : bool
    end

    module VDem : Vector_hetero.S2 with type ('k,'d) key = ('k,'d) Dem.t
                                    and type ('k,'d,'unneeded) data =
                                          (module Dem with type event = 'k and type runable = 'd)

    module RegisterDem : functor (D : Dem) -> sig  end

    val get_dem : ('a, 'b) dem -> ('a, 'b, unit) VDem.data

    val print_dem_event : ('a, 'b) dem -> Format.formatter -> 'a -> unit

    val print_dem_runable : ('a, 'b) dem -> Format.formatter -> 'b -> unit

    val new_pending_daemon : delayed -> ('a, 'b) dem -> 'b -> unit

    val wakeup_event : 'a translate -> delayed -> 'a -> t -> unit

    val wakeup_events_list :
      'a translate -> delayed -> t list option -> 'a -> unit

    val wakeup_events_bag :
      'a translate -> delayed -> t Bag.t option -> 'a -> unit

    val well_initialized : unit -> bool
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

      val key: (event,runable) dem
      val immediate: bool

    end

    module VDem = Dem.MkVector
        (struct type ('k,'d,'unedeed) t =
                  (module Dem with type event = 'k and type runable = 'd) end)

    let defined_dem : unit VDem.t = VDem.create 8

    module RegisterDem (D:Dem) = struct

      let () =
        VDem.inc_size D.key defined_dem;
        assert (if not (VDem.is_uninitialized defined_dem D.key)
                then raise AlreadyRegisteredKey else true);
        let dem =
          (module D: Dem with type event = D.event and type runable = D.runable) in
        VDem.set defined_dem D.key dem

    end

    let get_dem k =
      assert (if VDem.is_uninitialized defined_dem k
              then raise UnregisteredKey else true);
      VDem.get defined_dem k

    let print_dem_event (type k) (type d) (k : (k,d) dem) fmt s =
      let module S = (val get_dem k) in
      S.print_event fmt s

    let () = Print.pdem_event.Print.pdem_event <- print_dem_event

    let print_dem_runable (type k) (type d) (k : (k,d) dem) fmt s =
      let module S = (val get_dem k) in
      S.print_runable fmt s

    let () = Print.pdem_runable.Print.pdem_runable <- print_dem_runable

    let new_pending_daemon (type k) (type d) t (dem:(k,d) dem) runable =
      let module Dem = (val get_dem dem) in
      let daemonkey = DaemonKey(dem, runable) in
      if Dem.immediate
      then S.schedule_immediate t daemonkey
      else S.schedule t daemonkey

    let wakeup_event translate t info wevent =
      match wevent with
      | Event (dem,event) ->
        let rec f : type event r. S.delayed -> (event,r) dem -> event -> unit =
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
        Debug.dprintf0 debug "[Solver] @[No scheduling@]"
      | Some events ->
        List.iter (wakeup_event translate t info) events

    let wakeup_events_bag translate t events info =
      let is_empty = match events with
        | None -> true
        | Some events -> Bag.is_empty events in
      if is_empty then Debug.dprintf0 debug "[Solver] @[No scheduling@]"
      else Bag.iter (wakeup_event translate t info) (Opt.get events)

    let well_initialized () =
      let well_initialized = ref true in
      Dem.iter {Dem.iter = fun dem ->
          if VDem.is_uninitialized defined_dem dem then begin
            Format.eprintf
              "[Warning] The daemon %a is not registered" Dem.pp dem;
            well_initialized := false;
          end};
      !well_initialized


  end
end
