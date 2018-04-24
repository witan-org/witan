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

open Nodes

let debug = Debug.register_info_flag
  ~desc:"for the specialized demons"
  "Demon.all"


module Create = struct
    type 'b event =
    | EventDom      : Node.t * 'a Dom.t  * 'b -> 'b event
    | EventValue    : Node.t * 'a ValueKind.t * 'b -> 'b event
    | EventRegCl    : Node.t           * 'b -> 'b event
    | EventChange   : Node.t           * 'b -> 'b event
    | EventRegSem   :        'a ThTermKind.t  * 'b -> 'b event
    | EventRegValue :      'a ValueKind.t  * 'b -> 'b event


    let pp fmt = function
      | EventDom      (node, dom, _) ->
        Format.fprintf fmt "dom:%a of %a" Dom.pp dom Node.pp node
      | EventValue    (node, value, _) ->
        Format.fprintf fmt "value:%a of %a" ValueKind.pp value Node.pp node
      | EventRegCl  (node, _)    ->
        Format.fprintf fmt "registration of %a" Node.pp node
      | EventChange   (node, _)    ->
        Format.fprintf fmt "changecl of %a" Node.pp node
      | EventRegSem (sem, _)    ->
        Format.fprintf fmt "regsem for %a" ThTermKind.pp sem
      | EventRegValue (value, _)    ->
        Format.fprintf fmt "regvalue for %a" ValueKind.pp value


    type 'b t = 'b event list
end

type 'k alive =
| AliveReattached
| AliveStopped
| AliveRedirected of 'k

module Key = struct

  type ('d,'k,'i) daemon_state =
    | Alive of 'd Events.Fired.t * 'i
    | Dead
    | Redirected of 'k

  let print_daemon_state fmt = function
    | Alive _ -> Format.fprintf fmt "alive"
    | Dead -> Format.fprintf fmt "dead"
    | Redirected _ -> Format.fprintf fmt "redirected"

  module type DemTable = sig
    module Key: Stdlib.Datatype
    type data
    type info val default: info
    val state : (data, Key.t, info) daemon_state Key.M.t
  end

  type ('k,'d,'i) demtable =
    (module DemTable with type Key.t = 'k and type data = 'd
                                          and type info = 'i)

  type ('k,'d,'i) t = {
    dk_id : ('k * 'd, 'k) Events.Dem.t;
    dk_data : ('k,'d,'i) demtable Env.t;
  }

  let create (type k d i) name =
    { dk_id   = Events.Dem.create_key name;
      dk_data = (let module M = struct
                   type t = (k,d,i) demtable
                   let name = name
                 end
                 in Env.create_key (module M)) }

  module type S = sig
    module Key: Stdlib.Datatype

    module Data: Stdlib.Printable

    type info
    val default: info

    val key: (Key.t, Data.t, info) t

    val immediate: bool
    val wakeup:
      Egraph.t -> Key.t -> Data.t Events.Fired.t ->
      info -> Key.t alive
    (** the Events.t in wakeup is a subset of the one given in watch *)
  end

  (** mark it attached if it is not already the case *)
  let mark_dem :
  type k d i. Egraph.t -> (k,d,i) t -> k -> unit =
    fun d dem k ->
      try
        let module DemTable = (val (Egraph.get_env d dem.dk_data)) in
        let module DemTable' = struct
          include DemTable
          let state = DemTable.Key.M.change (function
              | None -> Some (Alive([],DemTable.default))
              | Some Dead -> raise AlreadyDead
              | Some (Redirected _) -> raise AlreadyRedirected
              | Some (Alive _) -> raise Exit)
              k DemTable.state
        end in
        Egraph.set_env d dem.dk_data (module DemTable')
      with Exit -> ()

  module Register(D:S) = struct

    let rec run d k =
      let module DemTable = (val (Egraph.get_env d D.key.dk_data)) in
      match DemTable.Key.M.find k (DemTable.state) with
      | Dead ->
        Debug.dprintf4 debug "[Demon] @[Daemon %a for %a is dead@]"
          Events.Dem.pp D.key.dk_id DemTable.Key.pp k;
        None
      | Redirected k' ->
        Debug.dprintf6 debug
        "[Demon] @[Daemon %a for %a is redirected to %a@]"
        Events.Dem.pp D.key.dk_id DemTable.Key.pp
        k DemTable.Key.pp k';
        run d k'
      | Alive (events,info) ->
        Debug.dprintf6 debug "[Demon] @[Run daemon %a for %a:@[%a@]@]"
          Events.Dem.pp D.key.dk_id DemTable.Key.pp k
          (Pp.list Pp.newline Events.Fired.pp) events;
        (** event can be added during wakeup *)
        let module DemTable' = struct
          include DemTable
          let state = DemTable.Key.M.add k (Alive([],info)) (DemTable.state)
        end
        in
        Egraph.set_env d D.key.dk_data (module DemTable');
        (** wakeup *)
        let alive = D.wakeup d k events info in
        (** delayed can be modified *)
        begin match alive with
          | AliveStopped | AliveRedirected _ ->
            let demstate = match alive with
              | AliveStopped -> Dead
              | AliveRedirected k' -> mark_dem d D.key k'; Redirected k'
              | AliveReattached -> assert false  in
            Debug.dprintf4 debug "[Demon] @[Stop daemon %a %a@]"
              Events.Dem.pp D.key.dk_id DemTable.Key.pp k;
            begin
              let module DemTable =
                (val (Egraph.get_env d D.key.dk_data)) in
              (** Dead even if event have been added *)
              let state' = DemTable.Key.M.add k demstate (DemTable.state) in
              let module DemTable' = struct
                include DemTable
                let state = state'
              end
              in
              Egraph.set_env d D.key.dk_data (module DemTable')
            end
          | AliveReattached ->
            Debug.dprintf0 debug "[Demon] @[Reattach daemon@]";
        end;
        None

    let enqueue d event =
      let module DemTable = (val (Egraph.Ro.get_env d D.key.dk_data)) in
      let change_state k l =
          Debug.dprintf6 debug
          "[Demon] @[schedule %a for %a with %a@]"
          Events.Dem.pp D.key.dk_id D.Key.pp k
          Events.Fired.pp event;
        let module DemTable' = struct
          include DemTable
          let state = DemTable.Key.M.add k l DemTable.state
        end in
        Egraph.Ro.set_env d D.key.dk_data (module DemTable')
      in
      let rec update_state k data =
        match DemTable.Key.M.find_opt k DemTable.state with
        | None -> assert false (* should have been marked *)
        | Some Dead ->
          Debug.dprintf4 debug
            "[Demon] @[Dem %a is dead for %a@]"
            Events.Dem.pp D.key.dk_id Events.Fired.pp event;
          Events.Wait.EnqStopped
        | Some (Redirected k') -> update_state k' data
        | (Some Alive([],info))  ->
          change_state k (Alive([data],info));
          Events.Wait.EnqRun k
        | Some Alive(l,info) ->
          change_state k (Alive(data::l,info));
          Events.Wait.EnqAlready
      in
      let k, event =
        let open Events.Fired in
        match event with
        | EventDom      (a, b , (k,d))   -> k, EventDom(a, b, d)
        | EventValue    (a, b , (k,d))   -> k, EventValue(a, b, d)
        | EventSem      (a, b, c, (k,d)) -> k, EventSem(a, b, c, d)
        | EventReg      (a, (k,d))       -> k, EventReg(a, d)
        | EventRegNode  (a, (k,d))       -> k, EventRegNode(a, d)
        | EventChange   (a, (k,d))       -> k, EventChange(a, d)
        | EventRegSem (a, (k,d))         -> k, EventRegSem(a, d)
        | EventRegValue (a, (k,d))       -> k, EventRegValue(a, d) in
      update_state k event


    let () =
      let print_demtable fmt (d: (D.Key.t,D.Data.t,D.info) demtable) =
        let module DT = (val d) in
        Pp.iter2 DT.Key.M.iter Pp.newline Pp.colon
          D.Key.pp print_daemon_state fmt DT.state
      in
      Env.register print_demtable D.key.dk_data;
    (** Interface for generic daemon *)
    let module Dem = struct
      type runable = D.Key.t
      let print_runable = D.Key.pp
      let run = run

      type event = D.Key.t * D.Data.t
      let print_event fmt (k,d) =
        Format.fprintf fmt "(%a: %a)" D.Key.pp k D.Data.pp d
      let enqueue = enqueue

      let key = D.key.dk_id
      let immediate = D.immediate
    end in
    Egraph.Wait.register_dem (module Dem)

    let init d =
      let module DemTable = struct
        module Key = D.Key
        type data = D.Data.t
        type info = D.info let default = D.default
        let state = Key.M.empty
      end in
      Egraph.set_env d D.key.dk_data (module DemTable);

  end

  let attach :
    type k d i. Egraph.t -> (k,d,i) t -> k -> d Create.t -> unit =
    fun t dem k events ->
      mark_dem t dem k;
    (** record waiters *)
      let iter ev =
      Debug.dprintf2 debug "[Demon] @[Attach event %a@]"
        Create.pp ev;
        match ev with
        | Create.EventDom (node,dom,data) ->
          Egraph.attach_dom t node dom dem.dk_id (k,data)
        | Create.EventValue (node,value,data) ->
          Egraph.attach_value t node value dem.dk_id (k,data)
        | Create.EventChange (node,data) ->
          Egraph.attach_node t node dem.dk_id (k,data)
        | Create.EventRegCl (node,data) ->
          Egraph.attach_reg_node t node dem.dk_id (k,data)
        | Create.EventRegSem (sem,data) ->
          Egraph.attach_reg_sem t sem dem.dk_id (k,data)
        | Create.EventRegValue (value,data) ->
          Egraph.attach_reg_value t value dem.dk_id (k,data)
      in
      List.iter iter events


  type ('k,'i) state =
  | SUnborn
  | SAlive of 'i
  | SDead
  | SRedirected of 'k

  let is_attached (type k) (type d) (type i) t (dem: (k,d,i) t) (k:k) =
    let module DemTable = (val (Egraph.get_env t dem.dk_data)) in
    match DemTable.Key.M.find_opt k DemTable.state with
    | None -> SUnborn
    | Some (Alive(_,i)) -> SAlive i
    | Some Dead -> SDead
    | Some (Redirected k') -> SRedirected k'

  exception NotAlive

  let set_info (type k) (type d) (type i) t (dem: (k,d,i) t) (k:k) (i:i)  =
    let module DemTable = (val (Egraph.get_env t dem.dk_data)) in
    match DemTable.Key.M.find_exn NotAlive k DemTable.state with
    | Alive(w,_) ->
      let module DemTable' = struct
        include DemTable
        let state = DemTable.Key.M.add k (Alive(w,i)) DemTable.state
      end
      in
      Egraph.set_env t dem.dk_data (module DemTable')
    | _ -> raise NotAlive


  exception CantBeKilled

  let kill (type k) (type d) (type i) t (dem: (k,d,i) t) (k:k) =
    try
      let module DemTable = (val (Egraph.get_env t dem.dk_data)) in
      Debug.dprintf4 debug "[Demon] @[Kill dem %a %a@]"
        Events.Dem.pp dem.dk_id DemTable.Key.pp k;
      let module DemTable' = struct
        include DemTable
        let state = DemTable.Key.M.change (function
          | Some Dead -> raise Exit
          | _ -> Some Dead)
          k DemTable.state
      end in
      Egraph.set_env t dem.dk_data (module DemTable')
    with Exit -> ()

end

module Fast = struct

  type 'd t = {
    dk_id : ('d, unit) Events.Dem.t;
    dk_data : 'd Events.Fired.event list Env.t;
    (** for throttling *)
    mutable dk_remaining: int; (** 0 if the demon is not the current one *)
    dk_current : 'd Events.Fired.event Queue.t; (** empty if idem *)
  }

  let create (type d) name
    = {
      dk_id   = Events.Dem.create_key name;
      dk_data = (
        let module M = struct
          type t = d Events.Fired.event list
          let name = name
        end
        in Env.create_key (module M));
      dk_remaining = 0;
      dk_current = Queue.create ();
    }

  module type S = sig

    module Data: sig
      type t
      val pp: t Pp.pp
    end

    val key: Data.t t

    (** never killed *)
    val immediate: bool
    val throttle: int (** todo int ref? *)
    (** number of time run in a row *)
    val wakeup: Egraph.t -> Data.t Events.Fired.event -> unit

  end


  module Register(D:S) = struct

    let run d () =
      assert (D.key.dk_remaining == 0);
      assert (Queue.is_empty D.key.dk_current);
      let rec last_rev q n = function
        | [] -> [],n
        | a::l ->
          let rem,n = last_rev q n l in
          if n > 0 then begin
            assert (rem == []);
            Queue.add a q;
            rem,(n-1)
          end
          else a::rem, n in
      let events = Egraph.get_env d D.key.dk_data in
      let events,n = last_rev D.key.dk_current D.throttle events in
      D.key.dk_remaining <- n;
      Egraph.set_env d D.key.dk_data events;
      let new_runable = if events != [] then Some () else None in
      let rec run_one () =
        if not (Queue.is_empty D.key.dk_current) then
          let event = Queue.pop D.key.dk_current in
          Debug.dprintf6 debug
            "[Demon] @[Run daemon fast %a:@[%a@ %a@]@]"
            Events.Dem.pp D.key.dk_id Events.Fired.pp event
            D.Data.pp (Events.Fired.get_data event);
          D.wakeup d event;
          Debug.dprintf0 debug "[Demon] @[Done@]";
          if not D.immediate then Egraph.flush_internal d;
          run_one () in
      try
        run_one ();
        assert (D.key.dk_remaining >= 0);
        assert (Queue.is_empty D.key.dk_current);
        D.key.dk_remaining <- 0;
        new_runable
      with exn -> (** Normally Contradiction *)
        assert (D.key.dk_remaining >= 0);
        D.key.dk_remaining <- 0;
        Queue.clear D.key.dk_current;
        raise exn

    let enqueue d event =
      assert (D.key.dk_remaining >= 0);
      if D.key.dk_remaining = 0 then
        let events = Egraph.Ro.get_env d D.key.dk_data in
        Debug.dprintf4 debug
          "[Demon] @[schedule %a for %a@]"
          Events.Dem.pp D.key.dk_id Events.Fired.pp event;
        Egraph.Ro.set_env d D.key.dk_data (event::events);
        if events = [] then Events.Wait.EnqRun () else Events.Wait.EnqAlready
      else begin
        Debug.dprintf4 debug
          "[Demon] @[schedule %a for %a now@]"
          Events.Dem.pp D.key.dk_id Events.Fired.pp event;
        Queue.add event D.key.dk_current;
        D.key.dk_remaining <- D.key.dk_remaining - 1;
        assert (D.key.dk_remaining >= 0);
        Events.Wait.EnqAlready
      end


    let () =
      let print_demtable fmt d =
        Pp.list Pp.comma Events.Fired.pp fmt d
      in
      Env.register print_demtable D.key.dk_data;
    (** Interface for generic daemon *)
    let module Dem = struct
      type runable = unit
      let print_runable = Stdlib.DUnit.pp
      let run = run

      type event = D.Data.t
      let print_event = D.Data.pp
      let enqueue = enqueue

      let key = D.key.dk_id
      let immediate = D.immediate
    end in
    Egraph.Wait.register_dem (module Dem)

    let init d =
      Egraph.set_env d D.key.dk_data [];

  end

  let attach d dem events =
    let open Create in
    List.iter (function
        | EventDom      (node,dom,data) ->
          Egraph.attach_dom d node dom dem.dk_id data
        | EventValue    (node,value,data) ->
          Egraph.attach_value d node value dem.dk_id data
        | EventRegCl  (node,data) ->
          Egraph.attach_reg_node d node dem.dk_id data
        | EventChange   (node,data) ->
          Egraph.attach_node d node dem.dk_id data
        | EventRegSem (sem,data) ->
          Egraph.attach_reg_sem d sem dem.dk_id data
        | EventRegValue (value,data) ->
          Egraph.attach_reg_value d value dem.dk_id data
      ) events

  let register_init_daemon
    (type a)
    ~name
    ?(immediate=false)
    ?(throttle=100)
    (thterm: (module Nodes.RegisteredThTerm with type t = a) )
    (f:Egraph.t -> a -> unit)
    (init_d:Egraph.t)
    =
    let module ThTerm = (val thterm) in
    let module DaemonInit = struct
      let key = create name
      module Data = Stdlib.DUnit
      let immediate = immediate
      let throttle = throttle
      let wakeup d = function
        | Events.Fired.EventRegSem(thterm,()) ->
          let thterm = ThTerm.coerce_thterm thterm in
          f d thterm
        | _ -> raise UnwaitedEvent
    end in
    let module RDaemonInit = Register(DaemonInit) in
    RDaemonInit.init init_d;
    attach init_d DaemonInit.key [Create.EventRegSem(ThTerm.key,())]

  let register_init_daemon_value
    (type a)
    ~name
    ?(immediate=false)
    ?(throttle=100)
    (value: (module Nodes.RegisteredValue with type t = a) )
    (f:Egraph.t -> a -> unit)
    (init_d:Egraph.t)
    =
    let module Val = (val value) in
    let module DaemonInit = struct
      let key = create name
      module Data = Stdlib.DUnit
      let immediate = immediate
      let throttle = throttle
      let wakeup d = function
        | Events.Fired.EventRegValue(value,()) ->
          let thterm = Val.coerce_nodevalue value in
          f d thterm
        | _ -> raise UnwaitedEvent
    end in
    let module RDaemonInit = Register(DaemonInit) in
    RDaemonInit.init init_d;
    attach init_d DaemonInit.key [Create.EventRegValue(Val.key,())]


end
