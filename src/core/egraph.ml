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

open Stdlib
open Nodes

exception Contradiction of Trail.Pexp.t

let debug = Debug.register_info_flag
  ~desc:"for the core solver"
  "Egraph.all"
let debug_few = Debug.register_info_flag
  ~desc:"for the core solver"
  "Egraph.few"

let stats_set_dom =
  Debug.register_stats_int ~name:"Egraph.set_dom/merge" ~init:0
let stats_set_value =
  Debug.register_stats_int ~name:"Egraph.set_value/merge" ~init:0

module DecTag = DInt

type 'a domtable = {
  table : 'a Node.M.t;
  events : Events.Wait.t Bag.t Node.M.t
}

type semtable = Events.Wait.t list

module VDomTable = Dom.MkVector (struct type ('a,'unused) t = 'a domtable end)

module VSemTable = ThTermKind.Vector

type 'a valuetable = {
  table : 'a Node.M.t;
  events : Events.Wait.t Bag.t Node.M.t;
  reg_events : Events.Wait.t list;
}
module VValueTable = ValueKind.MkVector (struct type ('a,'unit) t = 'a valuetable end)

(** Environnement *)

(** mutable but only contain persistent structure *)
(** Just for easy qualification *)
module Def = struct
type t = {
  mutable repr  : Node.t Node.M.t;
  mutable event : Events.Wait.t Bag.t Node.M.t;
  mutable event_reg : Events.Wait.t list Node.M.t;
  mutable event_any_reg : Events.Wait.t list;
          (** extensible "number of fields" *)
          dom   : delayed_t VDomTable.t;
          sem   : semtable VSemTable.t;
          value : unit VValueTable.t;
          envs  : unit Env.VectorH.t;
          trail : Trail.t;
  mutable current_delayed  : delayed_t; (** For assert-check *)
}

(** delayed_t is used *)
and delayed_t = {
  env : t;
  todo_immediate_dem : action_immediate_dem Queue.t;
  todo_merge_dom : action_merge_dom Queue.t;
  mutable todo_delayed_merge : (Trail.Pexp.t * Node.t * Node.t * bool) option;
  todo_merge : action_merge Queue.t;
  todo_ext_action : action_ext Queue.t;
  sched_daemon : Events.Wait.daemon_key -> unit;
  sched_decision : Trail.chogen -> unit;
}

and action_immediate_dem =
| RunDem : Events.Wait.daemon_key -> action_immediate_dem

and action_merge_dom =
| SetMergeDomNode  :
    Trail.Pexp.t * 'a Dom.t * Node.t * Node.t * bool -> action_merge_dom

and action_merge =
| Merge of Trail.Pexp.t * Node.t * Node.t

and action_ext =
| ExtDem         : Events.Wait.daemon_key  -> action_ext

end

include Def

(** {2 Define events} *)

module WaitDef = struct
  type delayed = delayed_t
  let schedule_immediate t d = Queue.push (RunDem d) t.todo_immediate_dem
  let schedule t d = t.sched_daemon d

  type delayed_ro = delayed_t
  let readonly x = x
end
module Wait : Events.Wait.S with type delayed = delayed_t and type delayed_ro = delayed_t =
  Events.Wait.Make(WaitDef)

(** {2 Define domain registration} *)
module VDom = Dom.Make(struct type delayed = delayed_t type pexp = Trail.Pexp.t end)
include VDom

let mk_dumb_delayed () = { env = Obj.magic 0;
                           todo_immediate_dem = Queue.create ();
                           todo_merge_dom = Queue.create ();
                           todo_delayed_merge = None;
                           todo_merge = Queue.create ();
                           todo_ext_action = Queue.create ();
                           sched_daemon   = (fun _ -> (assert false : unit));
                           (* should never be called *)
                           sched_decision = (fun _ -> (assert false : unit));
                         }

let dumb_delayed = mk_dumb_delayed ()
let unsat_delayed = mk_dumb_delayed ()


let new_t () = {
  repr = Node.M.empty;
  event = Node.M.empty;
  event_reg = Node.M.empty;
  event_any_reg = [];
  dom = VDomTable.create 5;
  sem = VSemTable.create 5;
  value = VValueTable.create 5;
  envs = Env.VectorH.create 5;
  trail = Trail.create ();
  current_delayed = dumb_delayed;
  }

let new_handle t =
  assert (t.current_delayed == dumb_delayed);
  {
  repr  = t.repr;
  event = t.event;
  event_reg = t.event_reg;
  event_any_reg = t.event_any_reg;
  dom = VDomTable.copy t.dom;
  sem = VSemTable.copy t.sem;
  value = VValueTable.copy t.value;
  envs = Env.VectorH.copy t.envs;
  trail = Trail.new_handle t.trail;
  current_delayed = t.current_delayed;
}

(** {2 Table access in the environment } *)

let get_table_dom t k =
  VDom.check_is_registered k;
  VDomTable.inc_size k t.dom;
  VDomTable.get_def t.dom k
    { table = Node.M.empty;
      events = Node.M.empty }

let get_table_sem t k =
  Nodes.check_thterm_registered k;
  VSemTable.inc_size k t.sem;
  ThTermKind.Vector.get_def t.sem k []

let get_table_value t k =
  Nodes.check_value_registered k;
  VValueTable.inc_size k t.value;
  VValueTable.get_def t.value k
    { table = Node.M.empty;
      events = Node.M.empty;
      reg_events = [];
    }

exception UninitializedEnv of Env.K.t

exception NotRegistered

(** Just used for being able to qualify these function on t *)
module T = struct
  let rec find t node =
    let node' = Node.M.find_exn NotRegistered node t.repr in
    if Node.equal node node' then node else
      let r = find t node' in
      t.repr <- Node.M.add node r t.repr;
      r

  let find_def t node =
    let node' = Node.M.find_def node node t.repr in
    if Node.equal node node' then node else
      let r = find t node' in
      t.repr <- Node.M.add node r t.repr;
      r

  let is_repr t node =
    try Node.equal (Node.M.find node t.repr) node
    with Not_found -> true

  let is_equal t node1 node2 =
    let node1 = find_def t node1 in
    let node2 = find_def t node2 in
    Node.equal node1 node2
end
open T

let get_direct_dom (type a) t (dom : a Dom.t) node =
  Node.M.find_opt node (get_table_dom t dom).table

let get_dom t dom node =
  let node = find_def t node in
  get_direct_dom t dom node

let get_direct_value t value node =
  Node.M.find_opt node (get_table_value t value).table

let get_value t value node =
  let node = find_def t node in
  get_direct_value t value node

let get_env : type a. t -> a Env.t -> a
  = fun t k ->
    Env.check_is_registered k;
    Env.VectorH.inc_size k t.envs;
    if Env.VectorH.is_uninitialized t.envs k then
      raise (UninitializedEnv (k :> Env.K.t))
    else
      Env.VectorH.get t.envs k

let set_env : type a. t -> a Env.t -> a -> unit
  = fun t k ->
    Env.check_is_registered k;
    Env.VectorH.inc_size k t.envs;
    Env.VectorH.set t.envs k


let is_registered t node =
  Node.M.mem node t.repr


(** {2 For debugging and display} *)
let _print_env fmt t =
  let printd (type a) dom fmt (domtable:a domtable) =
    Format.fprintf fmt "%a:@[%a@]" Dom.pp dom
      (Pp.iter2 Node.M.iter Pp.newline Pp.colon
         Node.pp (Bag.pp Pp.comma Events.Wait.pp))
      domtable.events
  in
  VDomTable.pp Pp.newline Pp.nothing
    {VDomTable.printk = Pp.nothing}
    {VDomTable.printd} fmt t.dom


let dot_to_escape = Str.regexp "[{}|<>]"

let escape_for_dot pp v =
  let s = Pp.string_of_wnl pp v in
  let s = Str.global_replace dot_to_escape "\\\\\\0" s in
  s

let output_graph filename t =
  let open Graph in
  let module G = struct
    include Imperative.Digraph.Concrete(Node)
    let graph_attributes _ = []
    let default_vertex_attributes _ = [`Shape `Record]
    let vertex_name node = string_of_int (Node.hash node)

    let pp fmt node =
      let iter_dom dom fmt (domtable: _ domtable) =
        try
          let s   = Node.M.find node domtable.table in
          Format.fprintf fmt "| {%a | %s}"
            Dom.pp dom (escape_for_dot (VDom.print_dom dom) s);
        with Not_found -> ()
      in
      let iter_value value fmt (valuetable: _ valuetable) =
        try
          let s   = Node.M.find node valuetable.table in
          Format.fprintf fmt "| {%a | %s}"
            ValueKind.pp value (escape_for_dot (print_value value) s);
        with Not_found -> ()
      in
      let print_ty fmt node =
        if is_repr t node
        then Format.fprintf fmt ": %a" Ty.pp (Node.ty node)
      in
      let print_sem fmt node =
        match Only_for_solver.thterm node with
        | None -> ()
        | Some thterm ->
          match Only_for_solver.sem_of_node thterm with
          | Only_for_solver.ThTerm(sem,v) ->
            let (module S) = get_thterm sem in
            Format.fprintf fmt "| {%a | %s}"
              ThTermKind.pp sem (escape_for_dot S.pp v)
      in
      Format.fprintf fmt "{%a %a %a %a %a}" (* "{%a | %a | %a}" *)
        Node.pp node
        print_ty node
        print_sem node
        (if is_repr t node
         then VDomTable.pp Pp.nothing Pp.nothing
             {VDomTable.printk=Pp.nothing}
             {VDomTable.printd=iter_dom}
         else Pp.nothing)
        t.dom
        (if is_repr t node
         then VValueTable.pp Pp.nothing Pp.nothing
             {VValueTable.printk=Pp.nothing}
             {VValueTable.printd=iter_value}
         else Pp.nothing)
        t.value

    let vertex_attributes node =
      let label = Pp.string_of_wnl pp node in
      [`Label label]
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph _ = None
  end in
  let g = G.create () in
  Node.M.iter (fun node1 node2 ->
      if Node.equal node1 node2
      then G.add_vertex g node1
      else G.add_edge g node1 (find_def t node2)) t.repr;
  let cout = open_out filename in
  let module Dot = Graphviz.Dot(G) in
  Dot.output_graph cout g;
  close_out cout

let show_graph = Debug.register_flag
  ~desc:"Show each step in a gui"
  "dotgui"

let draw_graph =
  let c = ref 0 in
  fun ?(force=false) t ->
    if force || Debug.test_flag show_graph then
      let dir = "debug_graph.tmp" in
      if not (Sys.file_exists dir) then Unix.mkdir dir 0o700;
      let filename = Format.sprintf "%s/debug_graph%i.dot" dir !c in
      incr c;
      Debug.dprintf1 Debug._true "[DotGui] output dot file: %s" filename;
      output_graph filename t


(** {2 Delayed} *)

module Delayed = struct
  open T
  type t = delayed_t

  let is_current_env t = t.env.current_delayed == t

  let find t node =
    assert (is_current_env t);
    find t.env node

  let find_def t node =
    assert (is_current_env t);
    find_def t.env node

  let is_repr t node =
    assert (is_current_env t);
    is_repr t.env node

  let is_equal t node1 node2 =
    assert (is_current_env t);
    is_equal t.env node1 node2

  let get_dom t dom node =
    assert (is_current_env t);
    get_dom t.env dom node

  let get_value t value node =
    assert (is_current_env t);
    get_value t.env value node

  let get_env t env =
    assert (is_current_env t);
    get_env t.env env

  let set_env t env v =
    assert (is_current_env t);
    set_env t.env env v

  let is_registered t node =
    assert (is_current_env t);
    is_registered t.env node

  let set_value_direct (type a) t (value : a ValueKind.t) node0 new_v =
    Debug.incr stats_set_value;
    let node = find t node0 in
    let valuetable = get_table_value t.env value in
    let valuetable = {
      valuetable with
      table = Node.M.add node new_v valuetable.table;
    } in
    VValueTable.set t.env.value value valuetable;
    let events = Node.M.find_opt node valuetable.events in
    Wait.wakeup_events_bag Events.Wait.translate_value t events (node,value)

  let add_pending_merge (t : t) pexp node node' =
    Debug.dprintf4 debug "[Egraph] @[add_pending_merge for %a and %a@]"
      Node.pp node Node.pp node';
    assert (is_registered t node);
    assert (is_registered t node');
    assert (not (Node.equal (find t node) (find t node')));
    assert (Ty.equal (Node.ty node) (Node.ty node'));
    (* Add the actual merge for later *)
    Queue.add (Merge (pexp,node,node')) t.todo_merge

  let check_no_dom t node =
    let foldi acc _dom (domtable: _ domtable) =
      acc &&
      not (Node.M.mem node domtable.table)
    in
    VDomTable.fold_initializedi {VDomTable.foldi} true t.env.dom

  let register t node =
    assert (is_current_env t);
    if not (is_registered t node) then begin
      if Debug.test_flag debug_few then begin
      match Only_for_solver.thterm node with
      | None ->
        Debug.dprintf2 debug "[Egraph] @[register %a@]" Node.pp node
      | Some thterm ->
        Debug.dprintf4 debug "[Egraph] @[register %a: %a@]"
          Node.pp node ThTerm.pp thterm
      end;
      assert ( check_no_dom t node );
      t.env.repr <- Node.M.add node node t.env.repr;
      (** reg_node *)
      let new_events, node_events = Node.M.find_remove node t.env.event_reg in
      t.env.event_reg <- new_events;
      Wait.wakeup_events_list Events.Wait.translate_regnode t node_events node;
      (** reg *)
      Wait.wakeup_events_list Events.Wait.translate_reg
        t (Some t.env.event_any_reg) node;
      (** reg_sem *)
      match Only_for_solver.open_node node with
      | Only_for_solver.ThTerm thterm ->
        begin match Only_for_solver.sem_of_node thterm with
        | Only_for_solver.ThTerm(sem,_) ->
          let reg_events = get_table_sem t.env sem in
          Wait.wakeup_events_list Events.Wait.translate_regsem
            t (Some reg_events) (thterm)
        end
      | Only_for_solver.Value nodevalue ->
        begin match Only_for_solver.value_of_node nodevalue with
        | Only_for_solver.Value(value,v) ->
          let valuetable = get_table_value t.env value in
          let reg_events = valuetable.reg_events in
          Wait.wakeup_events_list Events.Wait.translate_regvalue
            t (Some reg_events) (nodevalue);
          set_value_direct t value node v
        end
    end

  let set_semvalue_pending t pexp node0 node0' =
    let node = find t node0 in
    assert (Ty.equal (Node.ty node) (Node.ty node0'));
    begin
      if not (is_registered t node0') then begin
        register t node0';
        (* Here the important part of this function the representative
           is forced to be node. The goal is to not grow the number of
           classes that can be used for representative for
           termination.
        *)
        t.env.repr <- Node.M.add node0' node t.env.repr;
        let pexp = pexp () in
        Trail.add_merge_start t.env.trail pexp
          ~node1:node0 ~node2:node0'
          ~node1_repr:node ~node2_repr:node0'
          ~new_repr:node;
        Trail.add_merge_finish t.env.trail pexp
          ~node1:node0 ~node2:node0'
          ~node1_repr:node ~node2_repr:node0'
          ~new_repr:node;
        (** wakeup the daemons register_node *)
        let event, other_event = Node.M.find_remove node0' t.env.event in
        Wait.wakeup_events_bag Events.Wait.translate_change t other_event node0';
        t.env.event <- event
      end
      (** node' is already registered *)
      else if Node.equal node (find t node0') then
        (** if node is the representant of node' then we have nothing to do *)
        ()
      else
        (** merge node and node0' *)
        let pexp = pexp () in
        add_pending_merge t pexp node0 node0'
    end

  let set_sem_pending t pexp node0 thterm =
    let node0' = ThTerm.node thterm in
    let pexp () =
      Trail.mk_pexp t.env.trail Trail.exp_same_sem
        (ExpSameSem(pexp,node0,thterm)) in
    set_semvalue_pending t pexp node0 node0'

  let set_value_pending t pexp node0 nodevalue =
    let node0' = Value.node nodevalue in
    let pexp () =
      Trail.mk_pexp t.env.trail Trail.exp_same_sem
        (ExpSameValue(pexp,node0,nodevalue)) in
    set_semvalue_pending t pexp node0 node0'

  let set_dom_pending (type a) t (dom : a Dom.t) node0 new_v =
    Debug.incr stats_set_dom;
    let node = find t node0 in
    let domtable = (get_table_dom t.env dom) in
    let new_table = Node.M.add_opt node new_v domtable.table in
    let domtable = { domtable with table = new_table } in
    VDomTable.set t.env.dom dom domtable;
    let events = Node.M.find_opt node domtable.events in
    Wait.wakeup_events_bag Events.Wait.translate_dom t events (node,dom)

  let set_dom_premerge_pending (type a) t (dom : a Dom.t) ~from:_ node0 (new_v:a) =
    Debug.incr stats_set_dom;
    let node  = find t node0 in
    let domtable = (get_table_dom t.env dom) in
    let new_table = Node.M.add node new_v domtable.table in
    let domtable = { domtable with table = new_table } in
    VDomTable.set t.env.dom dom domtable;
    let events = Node.M.find_opt node domtable.events in
    Wait.wakeup_events_bag Events.Wait.translate_dom t events (node0,dom)

  let choose_repr a b = Shuffle.shuffle2 (a,b)

  let merge_dom_pending (type a) t pexp (dom : a Dom.t) node1_0 node2_0 inv =
    let node1 = find t node1_0 in
    let node2  = find t node2_0  in
    let domtable = (get_table_dom t.env dom) in
    let old_other_s = Node.M.find_opt node1 domtable.table in
    let old_repr_s = Node.M.find_opt node2  domtable.table in
    let (module Dom) = VDom.get_dom dom in
    Debug.dprintf12 debug_few
      "[Egraph] @[merge dom (%a(%a),%a)@ and (%a(%a),%a)@]"
      Node.pp node1 Node.pp node1_0
      (Pp.option Dom.pp) old_other_s
      Node.pp node2 Node.pp node2_0
      (Pp.option Dom.pp) old_repr_s;
    match old_other_s, old_repr_s with
    | None, None   -> ()
    | _ ->
      Dom.merge t pexp
        (old_other_s,node1_0)
        (old_repr_s,node2_0)
        inv


  let merge_dom ?(dry_run=false) t pexp node1_0 node2_0 inv =
    let node1 = find t node1_0 in
    let node2  = find t node2_0  in
    let dom_not_done = ref false in
    let iteri (type a) (dom : a Dom.t) (domtable : a domtable) =
      let s1 = Node.M.find_opt node1 domtable.table in
      let s2  = Node.M.find_opt node2  domtable.table in
    let (module Dom) = VDom.get_dom dom in
      if not (Dom.merged s1 s2)
      then begin
        dom_not_done := true;
        if not dry_run then
          Queue.push
            (SetMergeDomNode(pexp,dom,node1_0,node2_0,inv)) t.todo_merge_dom
      end
    in
    VDomTable.iter_initializedi {VDomTable.iteri} t.env.dom;
    !dom_not_done

  let merge_values t pexp node0 node0' =
    let node  = find t node0 in
    let node' = find t node0'  in
    let iteri (type a) (value:a ValueKind.t) (valuetable:a valuetable) =
      let old_s   = Node.M.find_opt node  valuetable.table in
      let old_s'  = Node.M.find_opt node' valuetable.table in
      let (module V) = Nodes.get_value value in
      Debug.dprintf12 debug
        "[Egraph] @[merge value (%a(%a),%a)@ and (%a(%a),%a)@]"
        Node.pp node Node.pp node0
        (Pp.option (print_value value)) old_s
        Node.pp node' Node.pp node0'
        (Pp.option (print_value value)) old_s';
      match old_s, old_s' with
      | None, None   -> ()
      | Some v, None ->
        set_value_direct t value node0' v
      | None, Some v' ->
        set_value_direct t value node0  v'
      | Some v, Some v' ->
        if V.equal v v'
        then
          (* already same value. Does that really happen? *)
          ()
        else
          let ty = Node.ty node in
          let v  = Value.index value v ty in
          let v' = Value.index value v' ty in
          let pexp = Trail.mk_pexp t.env.trail Trail.exp_diff_value (v,node0,node0',v',pexp) in
          raise (Contradiction pexp)
    in
    VValueTable.iter_initializedi {VValueTable.iteri} t.env.value

  let finalize_merge t pexp node1_0 node2_0 inv =
    let node1 = find t node1_0 in
    let node2  = find t node2_0  in
    let other_node0,other_node,repr_node0,repr_node =
      if inv
      then node2_0,node2, node1_0, node1
      else node1_0, node1, node2_0, node2 in
    merge_values t pexp node1_0 node2_0;
    t.env.repr <- Node.M.add other_node repr_node t.env.repr;
    Trail.add_merge_finish t.env.trail pexp
      ~node1:node1_0 ~node2:node2_0
      ~node1_repr:node1 ~node2_repr:node2
      ~new_repr:repr_node;
    Debug.dprintf10 debug_few "[Egraph.few] [%a] merge %a(%a) -> %a(%a)"
      Trail.print_current_age t.env.trail
      Node.pp other_node Node.pp other_node0
      Node.pp repr_node Node.pp repr_node0;
    let event, other_event = Node.M.find_remove other_node t.env.event in

    (** move node events *)
    begin match other_event with
      | None -> ()
      | Some other_event ->
        t.env.event <-
          Node.M.add_change (fun x -> x) Bag.concat repr_node other_event
            event
    end;

    (** move dom events  *)
    let iteri (type a) (dom : a Dom.t) (domtable: a domtable) =
      match Node.M.find_opt other_node domtable.events with
      | None -> ()
      | Some other_events ->
        let new_events =
          Node.M.add_change (fun x -> x) Bag.concat repr_node other_events
            domtable.events in
        let domtable = { domtable with events = new_events } in
        VDomTable.set t.env.dom dom domtable
    in
    VDomTable.iter_initializedi {VDomTable.iteri} t.env.dom;

    (** move value events  *)
    let iteri (type a) (value : a ValueKind.t) (valuetable: a valuetable) =
      match Node.M.find_opt other_node valuetable.events with
      | None -> ()
      | Some other_events ->
        let new_events =
          Node.M.add_change (fun x -> x) Bag.concat repr_node other_events
            valuetable.events in
        let valuetable = { valuetable with events = new_events } in
        VValueTable.set t.env.value value valuetable
    in
    VValueTable.iter_initializedi {VValueTable.iteri} t.env.value;

    (** wakeup the daemons *)
    Wait.wakeup_events_bag
      Events.Wait.translate_change t other_event other_node

  let do_delayed_merge t pexp node1_0 node2_0 inv  =
    let dom_not_done = merge_dom t pexp node1_0 node2_0 inv in
    if dom_not_done
    then begin
      Debug.dprintf4 debug "[Egraph] @[merge %a %a dom not done@]"
        Node.pp node1_0 Node.pp node2_0;
      t.todo_delayed_merge <- Some (pexp,node1_0,node2_0,inv)
    end
    else
      finalize_merge t pexp node1_0 node2_0 inv

  (** merge two pending actions *)
  let merge_pending t pexp node1_0 node2_0 =
    let node1 = find t node1_0 in
    let node2 = find t node2_0 in
    if not (Node.equal node1 node2) then begin
      let ((other_node0,_),(_,repr_node)) =
        choose_repr (node1_0,node1) (node2_0,node2) in
      let inv = not (Node.equal node1_0 other_node0) in
      Trail.add_merge_start t.env.trail pexp
        ~node1:node1_0 ~node2:node2_0
        ~node1_repr:node1 ~node2_repr:node2
        ~new_repr:repr_node;
      do_delayed_merge t pexp node1_0 node2_0 inv
    end

  (** {2 Internal scheduler} *)

  (**
     - Set dom, set value are done immediately
     - daemon immediate
     - merge domain
     - end merging (the class are really merged)
     - start merging (+ merge value)
     - daemon not immediate
  *)

  let rec do_pending_daemon delayed (Events.Wait.DaemonKey (dem,runable)) =
    let (module Dem) = Wait.get_dem dem in
    match Dem.run delayed runable with
    | None -> ()
    | Some runable -> Wait.new_pending_daemon delayed dem runable

  and nothing_todo t =
      Queue.is_empty t.todo_immediate_dem
    && Queue.is_empty t.todo_merge_dom
    && t.todo_delayed_merge == None
    && Queue.is_empty t.todo_merge
    && Queue.is_empty t.todo_ext_action

  and do_pending t =
    draw_graph t.env;
    if not (Queue.is_empty t.todo_immediate_dem) then
      match Queue.pop t.todo_immediate_dem with
      | RunDem att ->
        Debug.dprintf0 debug "[Egraph] @[do_pending RunDem immediate@]";
        do_pending_daemon t att;
        do_pending t
    else if not (Queue.is_empty t.todo_merge_dom) then
      match Queue.pop t.todo_merge_dom with
      | SetMergeDomNode(pexp,dom,node1,node2,inv) ->
        Debug.dprintf6 debug "[Egraph] @[do_pending SetDomNode %a %a %a@]"
          Dom.pp dom Node.pp node1 Node.pp node2;
        merge_dom_pending t pexp dom node1 node2 inv;
        do_pending t
    else match t.todo_delayed_merge with
      | Some(pexp,node1_0,node2_0,inv) ->
        t.todo_delayed_merge <- None;
        assert (not (merge_dom ~dry_run:true t pexp node1_0 node2_0 inv));
        (** understand why that happend.
            Is it really needed to do a fixpoint? *)
        do_delayed_merge t pexp node1_0 node2_0 inv;
        do_pending t
    | None ->
      if not (Queue.is_empty t.todo_merge) then
      match Queue.pop t.todo_merge with
      | Merge (pexp,node1,node2) ->
        Debug.dprintf4 debug "[Egraph] @[do_pending Merge %a %a@]"
          Node.pp node1 Node.pp node2;
        merge_pending t pexp node1 node2;
        do_pending t
    else if not (Queue.is_empty t.todo_ext_action) then
      (begin match Queue.pop t.todo_ext_action with
      | ExtDem att ->
        Debug.dprintf0 debug "[Egraph] @[do_pending RunDem@]";
        let store_ext_action = Queue.create () in
        Queue.transfer t.todo_ext_action store_ext_action;
        do_pending_daemon t att;
        Queue.transfer store_ext_action t.todo_ext_action;
       end;
       do_pending t)
    else
      Debug.dprintf0 debug "[Egraph] Nothing to do"

  and flush d =
    assert (d.env.current_delayed == d);
    Debug.dprintf0 debug "[Egraph] @[flush delayed@]";
    try
      if not (Queue.is_empty d.todo_ext_action) then
        let saved_ext_action = Queue.create () in
        Queue.transfer d.todo_ext_action saved_ext_action;
        do_pending d;
        Queue.transfer saved_ext_action d.todo_ext_action;
      else
        do_pending d;
      assert (nothing_todo d);
      Debug.dprintf0 debug "[Egraph] @[flush delayed end@]"
    with e when Debug.test_flag debug &&
                not (Debug.test_flag Debug.stack_trace) ->
      raise e

  (** {2 API} *)

  let merge t pexp node1_0 node2_0 =
    assert (is_current_env t);
    if not (Node.equal
              (find t node1_0)
              (find t node2_0)) then
      add_pending_merge t pexp node1_0 node2_0

  let check d node =
    assert (d.env.current_delayed == d);
    assert (is_registered d node)

  let set_sem  d pexp node thterm =
    Debug.dprintf4 debug "[Egraph] @[add_pending_set_sem for %a and %a@]"
      Node.pp node ThTerm.pp thterm;
    check d node;
    set_sem_pending d pexp node thterm

  let set_nodevalue  d pexp node nodevalue =
    Debug.dprintf4 debug "[Egraph] @[add_pending_set_nodevalue for %a and %a@]"
      Node.pp node Value.pp nodevalue;
    check d node;
    set_value_pending d pexp node nodevalue

  let set_values d pexp node nodevalue =
    Debug.dprintf4 debug_few
      "[Egraph] @[set_value for %a with %a@]"
      Node.pp node Value.pp nodevalue;
    set_value_pending d pexp node nodevalue

  let set_value (type a)  d pexp (value : a ValueKind.t) node v =
    let nodevalue = Value.index value v (Node.ty node) in
    set_values d pexp node nodevalue

  let set_dom d dom node v =
    Debug.dprintf4 debug_few
      "[Egraph] @[set_dom for %a with %a@]"
      Node.pp node (print_dom dom) v;
    check d node;
    set_dom_pending d dom node (Some v)

  let set_dom_premerge d dom node v =
    Debug.dprintf4 debug
      "[Egraph] @[set_dom_premerge for %a with %a@]"
      Node.pp node (print_dom dom) v;
    check d node;
    let node' = match d.todo_delayed_merge with
    | Some(_,node1,node2,_) when Node.equal node1 node -> node2
    | Some(_,node1,node2,_) when Node.equal node2 node -> node1
    | _ -> raise (BrokenInvariant(
        "set_dom_premerge should be used only on the \
         nodeasses currently merged")) in
    set_dom_premerge_pending d dom ~from:node' node v

  let unset_dom d dom node =
    Debug.dprintf2 debug
      "[Egraph] @[unset_dom for %a@]"
      Node.pp node;
    check d node;
    set_dom_pending d dom node None

  let register_decision t chogen =
    t.sched_decision chogen

  let mk_pexp t ?age kexp exp = Trail.mk_pexp ?age t.env.trail kexp exp
  let current_age t = Trail.current_age t.env.trail
  let add_pexp t pexp = Trail.add_pexp t.env.trail pexp

  let contradiction d pexp =
    d.env.current_delayed <- unsat_delayed;
    raise (Contradiction pexp)

  (** {2 API for attaching event} *)

  let attach_dom (type a) t node (dom : a Dom.t) dem event =
    let node = find_def t node in
    let event = Events.Wait.Event (dem,event) in
    let domtable = get_table_dom t.env dom in
    let domtable = {
      domtable with
      events = Node.M.add_change Bag.elt Bag.add node event domtable.events
    }
    in
    VDomTable.set t.env.dom dom domtable

  let attach_value (type a) t node (value : a ValueKind.t) dem event =
    let node = find_def t node in
    let event = Events.Wait.Event (dem,event) in
    let valuetable = (get_table_value t.env value) in
    let valuetable = {
      valuetable with
      events = Node.M.add_change Bag.elt Bag.add node event valuetable.events
    } in
    VValueTable.set t.env.value value valuetable

  let attach_node t node dem event =
    let node = find_def t node in
    let event = Events.Wait.Event (dem,event) in
    t.env.event <- Node.M.add_change Bag.elt Bag.add node event t.env.event

  let attach_reg_node t node dem event =
    let event = Events.Wait.Event (dem,event) in
    match find t node with
    | node -> (** already registered *)
      Wait.wakeup_events_list Events.Wait.translate_regnode t (Some [event]) node
    | exception NotRegistered ->
      t.env.event_reg <-
        Node.M.add_change Lists.singleton Lists.add node event t.env.event_reg

  let attach_reg_sem (type a) t (sem : a ThTermKind.t) dem event =
    let event = Events.Wait.Event (dem,event) in
    let reg_events = get_table_sem t.env sem in
    let reg_events = event::reg_events in
    ThTermKind.Vector.set t.env.sem sem reg_events

  let attach_reg_value (type a) t (value : a ValueKind.t) dem event =
    let event = Events.Wait.Event (dem,event) in
    let value_table = get_table_value t.env value in
    let reg_events = event::value_table.reg_events in
    VValueTable.set t.env.value value {value_table with reg_events}

  let attached_reg_node
      (type k) (type d) d node (dem:(k,d) Events.Dem.t) : k Enum.t =
    Enum.from_list
      ~filter:(function
          | Events.Wait.Event(dem',_) ->
            Events.Dem.equal dem dem'
        )
      ~map:(function
          | Events.Wait.Event(dem',event) ->
            match Events.Dem.Eq.coerce_type dem dem' with
            | Keys.Eq, Keys.Eq -> (event:k)
        )
      (Node.M.find_def [] node d.env.event_reg)

  let attached_node
      (type k) (type d) d node (dem:(k,d) Events.Dem.t) : k Enum.t =
    Enum.from_bag
      ~filter:(function
          | Events.Wait.Event(dem',_) ->
            Events.Dem.equal dem dem'
        )
      ~map:(function
          | Events.Wait.Event(dem',event) ->
            match Events.Dem.Eq.coerce_type dem dem' with
            | Keys.Eq, Keys.Eq -> (event:k)
        )
      (Node.M.find_def Bag.empty node d.env.event)


end

let new_delayed ~sched_daemon ~sched_decision t =
  assert (t.current_delayed == dumb_delayed);
  let d =  { env = t;
             todo_immediate_dem = Queue.create ();
             todo_merge_dom = Queue.create ();
             todo_delayed_merge = None;
             todo_merge = Queue.create ();
             todo_ext_action = Queue.create ();
             sched_daemon; sched_decision;
           } in
  t.current_delayed <- d;
  d

let delayed_stop d =
  assert (d.env.current_delayed == d);
  assert (Delayed.nothing_todo d);
  d.env.current_delayed <- dumb_delayed

let flush d =
  assert (d.env.current_delayed == d);
  Delayed.do_pending d;
  assert (Delayed.nothing_todo d)

let run_daemon d dem =
  Queue.push (ExtDem dem) d.todo_ext_action

let is_equal t node1 node2 =
  assert (t.current_delayed == dumb_delayed);
  let node1,node2 = Shuffle.shuffle2 (node1,node2) in
  Debug.dprintf4 debug "[Egraph] @[is_equal %a %a@]"
    Node.pp node1 Node.pp node2;
  draw_graph t;
  is_equal t node1 node2

let find t node =
  assert (t.current_delayed == dumb_delayed);
  find t node

let get_dom t dom node =
  assert (t.current_delayed == dumb_delayed);
  get_dom t dom node

let get_value t value node =
  assert (t.current_delayed == dumb_delayed);
  get_value t value node

let get_env t env =
  assert (t.current_delayed == dumb_delayed);
  get_env t env

let set_env t env v =
  assert (t.current_delayed == dumb_delayed);
  set_env t env v

let is_repr t node =
  assert (t.current_delayed == dumb_delayed);
  is_repr t node

let find_def t node =
  assert (t.current_delayed == dumb_delayed);
  find_def t node

let get_trail t =
  (* assert (t.current_delayed == dumb_delayed ||
   *         t.current_delayed == unsat_delayed); *)
  t.trail

let get_getter t =
  (* assert (t.current_delayed == dumb_delayed); *)
  t.current_delayed


let new_dec t =
  assert (t.current_delayed == dumb_delayed);
  Trail.new_dec t.trail

let current_age (t:t) = Trail.current_age t.trail
let current_nbdec (t:t) = Trail.nbdec t.trail

let get_direct_dom t dom node =
  assert (t.current_delayed == dumb_delayed ||
          t.current_delayed == unsat_delayed);
  get_direct_dom t dom node


module type Getter = sig
  type t

  val is_equal  : t -> Node.t -> Node.t -> bool
  val find_def  : t -> Node.t -> Node.t
  val get_dom   : t -> 'a Dom.t -> Node.t -> 'a option
    (** dom of the nodeass *)
  val get_value : t -> 'a ValueKind.t -> Node.t -> 'a option
    (** value of the nodeass *)

  (** {4 The nodeasses must have been marked has registered} *)

  val find      : t -> Node.t -> Node.t
  val is_repr   : t -> Node.t -> bool

  val is_registered : t -> Node.t -> bool

  val get_env : t -> 'a Env.t -> 'a
  val set_env : t -> 'a Env.t -> 'a -> unit

end

module Getter : Getter with type t = Delayed.t = Delayed

module type Ro = sig
  type t = private Getter.t
  include Getter with type t := t

  (** {3 Immediate information} *)
  val register : t -> Node.t -> unit

  val is_current_env: t -> bool

end

module Ro : Ro with type t = Delayed.t = Delayed

let check_initialization () =
  VDom.is_well_initialized () && Wait.is_well_initialized ()

let () = Exn_printer.register (fun fmt exn ->
    match exn with
    | UninitializedEnv env ->
      Format.fprintf fmt "The environnement of %a is not initialized."
        Env.K.pp env
    | exn -> raise exn
  )
