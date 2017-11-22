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

open Stdlib
open Typedef
open Explanation

exception Contradiction of Explanation.pexp

let debug = Debug.register_info_flag
  ~desc:"for the core solver"
  "Solver.all"
let debug_few = Debug.register_info_flag
  ~desc:"for the core solver"
  "Solver.few"

let stats_set_dom =
  Debug.register_stats_int ~name:"Solver.set_dom/merge" ~init:0
let stats_set_value =
  Debug.register_stats_int ~name:"Solver.set_value/merge" ~init:0

type exp_same_sem =
| ExpSameSem   : pexp * Cl.t * ClSem.t -> exp_same_sem
| ExpSameValue : pexp * Cl.t * ClValue.t -> exp_same_sem

let exp_same_sem : exp_same_sem Explanation.exp =
  Explanation.Exp.create_key "Solver.exp_same_sem"

(** TODO choose an appropriate data *)
let exp_init_value : unit Explanation.exp =
  Explanation.Exp.create_key "Solver.exp_init_value"

(** TODO choose an appropriate data *)
let exp_diff_value : pexp Explanation.exp =
  Explanation.Exp.create_key "Solver.exp_diff_value"

module DecTag = DInt

type 'a domtable = {
  table : 'a Cl.M.t;
  events : Events.Wait.t Bag.t Cl.M.t
}

type semtable = Events.Wait.t list

module VDomTable = Dom.MkVector (struct type ('a,'unused) t = 'a domtable end)

module VSemTable = Sem.Vector

type 'a valuetable = {
  table : 'a Cl.M.t;
  events : Events.Wait.t Bag.t Cl.M.t;
  reg_events : Events.Wait.t list;
}
module VValueTable = Value.MkVector (struct type ('a,'unit) t = 'a valuetable end)

(** Environnement *)

(** mutable but only contain persistent structure *)
(** Just for easy qualification *)
module Def = struct
type t = {
  mutable repr  : Cl.t Cl.M.t;
  mutable event : Events.Wait.t Bag.t Cl.M.t;
  mutable event_reg : Events.Wait.t list Cl.M.t;
  mutable event_any_reg : Events.Wait.t list;
          (** extensible "number of fields" *)
          dom   : delayed_t VDomTable.t;
          sem   : semtable VSemTable.t;
          value : unit VValueTable.t;
          envs  : unit Env.VectorH.t;
          trail : Explanation.t;
  mutable current_delayed  : delayed_t; (** For assert-check *)
}

(** delayed_t is used *)
and delayed_t = {
  env : t;
  todo_immediate_dem : action_immediate_dem Queue.t;
  todo_merge_dom : action_merge_dom Queue.t;
  mutable todo_delayed_merge : (pexp * Cl.t * Cl.t * bool) option;
  todo_merge : action_merge Queue.t;
  todo_ext_action : action_ext Queue.t;
  sched_daemon : Events.Wait.daemon_key -> unit;
  sched_decision : chogen -> unit;
}

and action_immediate_dem =
| RunDem : Events.Wait.daemon_key -> action_immediate_dem

and action_merge_dom =
| SetMergeDomCl  :
    pexp * 'a Dom.t * Cl.t * Cl.t * bool -> action_merge_dom

and action_merge =
| Merge of pexp * Cl.t * Cl.t

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
module VDom = Dom.Make(struct type delayed = delayed_t type pexp = Explanation.pexp end)
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
  repr = Cl.M.empty;
  event = Cl.M.empty;
  event_reg = Cl.M.empty;
  event_any_reg = [];
  dom = VDomTable.create 5;
  sem = VSemTable.create 5;
  value = VValueTable.create 5;
  envs = Env.VectorH.create 5;
  trail = Explanation.create ();
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
  trail = Explanation.new_handler t.trail;
  current_delayed = t.current_delayed;
}

(** {2 Table access in the environment } *)

let get_table_dom t k =
  VDom.check_is_registered k;
  VDomTable.inc_size k t.dom;
  if VDomTable.is_uninitialized t.dom k then
    { table = Cl.M.empty;
      events = Cl.M.empty }
  else
    VDomTable.get t.dom k

let get_table_sem t k =
  assert (if sem_uninitialized k then raise UnregisteredKey else true);
  VSemTable.inc_size k t.sem;
  if VSemTable.is_uninitialized t.sem k
  then begin Sem.Vector.set t.sem k []; [] end
  else Sem.Vector.get t.sem k

let get_table_value t k =
  assert (if value_uninitialized k
    then raise UnregisteredKey else true);
  VValueTable.inc_size k t.value;
  if VValueTable.is_uninitialized t.value k then
    { table = Cl.M.empty;
      events = Cl.M.empty;
      reg_events = [];
    }
  else
    VValueTable.get t.value k

exception UninitializedEnv of Env.K.t

exception NotNormalized

(** Just used for being able to qualify these function on t *)
module T = struct
  let rec find t cl =
    let cl' = Cl.M.find_exn NotNormalized cl t.repr in
    if Cl.equal cl cl' then cl else
      let r = find t cl' in
      t.repr <- Cl.M.add cl r t.repr;
      r

  let find_def t cl =
    let cl' = Cl.M.find_def cl cl t.repr in
    if Cl.equal cl cl' then cl else
      let r = find t cl' in
      t.repr <- Cl.M.add cl r t.repr;
      r

  let is_repr t cl =
    try Cl.equal (Cl.M.find cl t.repr) cl
    with Not_found -> true

  let is_equal t cl1 cl2 =
    let cl1 = find_def t cl1 in
    let cl2 = find_def t cl2 in
    Cl.equal cl1 cl2
end
open T

let get_direct_dom (type a) t (dom : a Dom.t) cl =
  Cl.M.find_opt cl (get_table_dom t dom).table

let get_dom t dom cl =
  let cl = find_def t cl in
  get_direct_dom t dom cl

let get_direct_value t value cl =
  Cl.M.find_opt cl (get_table_value t value).table

let get_value t value cl =
  let cl = find_def t cl in
  get_direct_value t value cl

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


let is_registered t cl =
  Cl.M.mem cl t.repr


(** {2 For debugging and display} *)
let _print_env fmt t =
  let printd (type a) dom fmt (domtable:a domtable) =
    Format.fprintf fmt "%a:@[%a@]" Dom.pp dom
      (Pp.iter2 Cl.M.iter Pp.newline Pp.colon
         Cl.pp (Bag.pp Pp.comma Events.Wait.pp))
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
    include Imperative.Digraph.Concrete(Cl)
    let graph_attributes _ = []
    let default_vertex_attributes _ = [`Shape `Record]
    let vertex_name cl = string_of_int (Cl.hash cl)

    let pp fmt cl =
      let iter_dom dom fmt (domtable: _ domtable) =
        try
          let s   = Cl.M.find cl domtable.table in
          Format.fprintf fmt "| {%a | %s}"
            Dom.pp dom (escape_for_dot (VDom.print_dom dom) s);
        with Not_found -> ()
      in
      let print_ty fmt cl =
        if is_repr t cl
        then Format.fprintf fmt ": %a" Ty.pp (Cl.ty cl)
      in
      let print_sem fmt cl =
        match Only_for_solver.clsem cl with
        | None -> ()
        | Some clsem ->
          match Only_for_solver.sem_of_cl clsem with
          | Only_for_solver.Sem(sem,v) ->
            let module S = (val get_sem sem) in
            Format.fprintf fmt "| {%a | %s}"
              Sem.pp sem (escape_for_dot S.pp v)
      in
      Format.fprintf fmt "{%a %a %a %a}" (* "{%a | %a | %a}" *)
        Cl.pp cl
        print_ty cl
        print_sem cl
        (if is_repr t cl
         then VDomTable.pp Pp.nothing Pp.nothing
             {VDomTable.printk=Pp.nothing}
             {VDomTable.printd=iter_dom}
         else Pp.nothing)
        t.dom

    let vertex_attributes cl =
      let label = Pp.string_of_wnl pp cl in
      [`Label label]
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph _ = None
  end in
  let g = G.create () in
  Cl.M.iter (fun cl1 cl2 ->
      if Cl.equal cl1 cl2
      then G.add_vertex g cl1
      else G.add_edge g cl1 (find_def t cl2)) t.repr;
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
      let filename = Format.sprintf "debug_graph.tmp/debug_graph%i.dot" !c in
      incr c;
      Debug.dprintf1 Debug._true "[DotGui] output dot file: %s" filename;
      output_graph filename t


(** {2 Delayed} *)

module Delayed = struct
  open T
  type t = delayed_t

  let is_current_env t = t.env.current_delayed == t

  let find t cl =
    assert (is_current_env t);
    find t.env cl

  let find_def t cl =
    assert (is_current_env t);
    find_def t.env cl

  let is_repr t cl =
    assert (is_current_env t);
    is_repr t.env cl

  (* let is_repr_of t cl1 cl2 = *)
  (*   try Cl.equal (find t cl2) cl1 *)
  (*   with NotNormalized -> Cl.equal cl2 cl1 *)

  let is_equal t cl1 cl2 =
    assert (is_current_env t);
    is_equal t.env cl1 cl2


  let is_registered t cl =
    assert (is_current_env t);
    is_registered t.env cl

  let set_value_direct (type a) t pexp (value : a value) cl0 new_v =
    Debug.incr stats_set_value;
    let cl = find t cl0 in
    let valuetable = get_table_value t.env value in
    let events = Cl.M.find_opt cl valuetable.events in
    let new_table = Cl.M.add cl new_v valuetable.table in
    let valuetable = {
      valuetable with
      table = new_table
    } in
    VValueTable.set t.env.value value valuetable;
    Explanation.add_pexp_value t.env.trail pexp value ~cl ~cl0;
    Wait.wakeup_events_bag Events.Wait.translate_value t events (cl,value)

  let merge_values t pexp cl0 cl0' =
    let cl  = find t cl0 in
    let cl' = find t cl0'  in
    let iteri (type a) (value:a value) (valuetable:a valuetable) =
      let old_s = Cl.M.find_opt cl valuetable.table in
      let old_s'  = Cl.M.find_opt cl'  valuetable.table in
      let module Value = (val (Typedef.get_value value)) in
      Debug.dprintf12 debug_few
        "[Solver] @[merge value (%a(%a),%a)@ and (%a(%a),%a)@]"
        Cl.pp cl Cl.pp cl0
        (Pp.option (print_value value)) old_s
        Cl.pp cl' Cl.pp cl0'
        (Pp.option (print_value value)) old_s';
      match old_s, old_s' with
      | None, None   -> ()
      | Some v, None ->
        set_value_direct t pexp value cl0' v
      | None, Some v' ->
        set_value_direct t pexp value cl0  v'
      | Some v, Some v' ->
        if Value.equal v v'
        then
          (* already same value. Does that really happen? *)
          ()
        else
          let pexp =
            mk_pexp t.env.trail exp_diff_value pexp in
          raise (Contradiction(pexp))
    in
    VValueTable.iter_initializedi {VValueTable.iteri} t.env.value

  let add_pending_merge (t : t) pexp cl cl' =
    Debug.dprintf4 debug "[Solver] @[add_pending_merge for %a and %a@]"
      Cl.pp cl Cl.pp cl';
    assert (is_registered t cl);
    assert (is_registered t cl');
    assert (not (Cl.equal (find t cl) (find t cl')));
    assert (Ty.equal (Cl.ty cl) (Cl.ty cl'));
    (*  Immediately merge values *)
    merge_values t pexp cl cl';
    (* Add the actual merge for later *)
    Queue.add (Merge (pexp,cl,cl')) t.todo_merge


  let get_dom t dom cl =
    assert (is_current_env t);
    get_dom t.env dom cl

  let get_value t value cl =
    assert (is_current_env t);
    get_value t.env value cl

  let get_env t env =
    assert (is_current_env t);
    get_env t.env env

  let set_env t env v =
    assert (is_current_env t);
    set_env t.env env v

  let attach_dom (type a) t cl (dom : a Dom.t) dem event =
    let cl = find_def t cl in
    let event = Events.Wait.Event (dem,event) in
    let domtable = get_table_dom t.env dom in
    let domtable = {
      domtable with
      events = Cl.M.add_change Bag.elt Bag.add cl event domtable.events
    }
    in
    VDomTable.set t.env.dom dom domtable

  let attach_value (type a) t cl (value : a value) dem event =
    let cl = find_def t cl in
    let event = Events.Wait.Event (dem,event) in
    let valuetable = (get_table_value t.env value) in
    let valuetable = {
      valuetable with
      events = Cl.M.add_change Bag.elt Bag.add cl event valuetable.events
    } in
    VValueTable.set t.env.value value valuetable

  let attach_cl t cl dem event =
    let cl = find_def t cl in
    let event = Events.Wait.Event (dem,event) in
    t.env.event <- Cl.M.add_change Bag.elt Bag.add cl event t.env.event

  let attach_reg_cl t cl dem event =
    let event = Events.Wait.Event (dem,event) in
    begin try
        let cl = find t cl in
        (** already registered *)
        Wait.wakeup_events_list Events.Wait.translate_regcl t (Some [event]) cl
      with NotNormalized ->
        t.env.event_reg <-
          Cl.M.add_change Lists.singleton Lists.add cl event t.env.event_reg
    end

  let attach_reg_sem (type a) t (sem : a sem) dem event =
    let event = Events.Wait.Event (dem,event) in
    let reg_events = get_table_sem t.env sem in
    let reg_events = event::reg_events in
    Sem.Vector.set t.env.sem sem reg_events

  let attached_reg_cl
      (type k) (type d) d cl (dem:(k,d) dem) : k Enum.t =
    Enum.from_list
      ~filter:(function
          | Events.Wait.Event(dem',_) ->
            Dem.equal dem dem'
        )
      ~map:(function
          | Events.Wait.Event(dem',event) ->
            match Dem.Eq.coerce_type dem dem' with
            | Keys.Eq, Keys.Eq -> (event:k)
        )
       (Cl.M.find_def [] cl d.env.event_reg)

  let attached_cl
    (type k) (type d) d cl (dem:(k,d) dem) : k Enum.t =
    Enum.from_bag
      ~filter:(function
          | Events.Wait.Event(dem',_) ->
            Dem.equal dem dem'
        )
      ~map:(function
          | Events.Wait.Event(dem',event) ->
            match Dem.Eq.coerce_type dem dem' with
            | Keys.Eq, Keys.Eq -> (event:k)
        )
       (Cl.M.find_def Bag.empty cl d.env.event)


(** *)


  let check_no_dom t cl =
    let foldi acc _dom (domtable: _ domtable) =
      acc &&
      not (Cl.M.mem cl domtable.table)
    in
    VDomTable.fold_initializedi {VDomTable.foldi} true t.env.dom

  let register t cl =
    assert (is_current_env t);
    if not (is_registered t cl) then begin
      if Debug.test_flag debug_few then begin
      match Only_for_solver.clsem cl with
      | None ->
        Debug.dprintf2 debug_few "[Solver] @[register %a@]" Cl.pp cl
      | Some clsem ->
        Debug.dprintf4 debug_few "[Solver] @[register %a: %a@]"
          Cl.pp cl ClSem.pp clsem
      end;
      assert ( check_no_dom t cl );
      t.env.repr <- Cl.M.add cl cl t.env.repr;
      (** reg_cl *)
      let new_events, cl_events = Cl.M.find_remove cl t.env.event_reg in
      t.env.event_reg <- new_events;
      Wait.wakeup_events_list Events.Wait.translate_regcl t cl_events cl;
      (** reg *)
      Wait.wakeup_events_list Events.Wait.translate_reg
        t (Some t.env.event_any_reg) cl;
      (** reg_sem *)
      match Only_for_solver.open_cl cl with
      | Only_for_solver.Fresh -> ()
      | Only_for_solver.Fresh_to_reg(dem,event) ->
        Wait.wakeup_events_list Events.Wait.translate_regcl t
          (Some [Events.Wait.Event(dem,event)])
          cl;
      | Only_for_solver.Sem clsem ->
        begin match Only_for_solver.sem_of_cl clsem with
        | Only_for_solver.Sem(sem,_) ->
          let reg_events = get_table_sem t.env sem in
          Wait.wakeup_events_list Events.Wait.translate_regsem
            t (Some reg_events) (clsem)
        end
      | Only_for_solver.Value clvalue ->
        begin match Only_for_solver.value_of_cl clvalue with
        | Only_for_solver.Value(value,v) ->
          let valuetable = get_table_value t.env value in
          let reg_events = valuetable.reg_events in
          Wait.wakeup_events_list Events.Wait.translate_regvalue
            t (Some reg_events) (clvalue);
          set_value_direct t (assert false (** TODO *)) value cl v
        end
    end

  let set_semvalue_pending t pexp cl0 cl0' =
    let cl = find t cl0 in
    assert (Ty.equal (Cl.ty cl) (Cl.ty cl0'));
    begin
      if not (is_registered t cl0') then begin
        register t cl0';
        (* Here the important part of this function the representative
           is forced to be cl. The goal is to not grow the number of
           classes that can be used for representative for
           termination.
        *)
        t.env.repr <- Cl.M.add cl0' cl t.env.repr;
        let pexp = pexp () in
        Explanation.add_pexp_cl t.env.trail pexp ~inv:true
          ~other_cl:cl0' ~other_cl0:cl0'
          ~repr_cl:cl ~repr_cl0:cl0;
        Explanation.add_merge_dom_no
          t.env.trail ~inv:true
          ~other_cl:cl0' ~other_cl0:cl0'
          ~repr_cl:cl ~repr_cl0:cl0;
        (** wakeup the daemons register_cl *)
        let event, other_event = Cl.M.find_remove cl0' t.env.event in
        Wait.wakeup_events_bag Events.Wait.translate_change t other_event cl0';
        t.env.event <- event
      end
      (** cl' is already registered *)
      else if Cl.equal cl (find t cl0') then
        (** if cl is the representant of cl' then we have nothing to do *)
        ()
      else
        (** merge cl and cl0' *)
        let pexp = pexp () in
        add_pending_merge t pexp cl0 cl0'
    end

  let set_sem_pending t pexp cl0 clsem =
    let cl0' = ClSem.cl clsem in
    let pexp () =
      mk_pexp t.env.trail exp_same_sem
        (ExpSameSem(pexp,cl0,clsem)) in
    set_semvalue_pending t pexp cl0 cl0'

  let set_value_pending t pexp cl0 clvalue =
    let cl0' = ClValue.cl clvalue in
    let pexp () =
      mk_pexp t.env.trail exp_same_sem
        (ExpSameValue(pexp,cl0,clvalue)) in
    set_semvalue_pending t pexp cl0 cl0'

  let set_dom_pending (type a) t pexp (dom : a Dom.t) cl0 new_v =
    Debug.incr stats_set_dom;
    let cl = find t cl0 in
    let domtable = (get_table_dom t.env dom) in
    let events = Cl.M.find_opt cl domtable.events in
    let new_table = Cl.M.add_opt cl new_v domtable.table in
    let domtable = { domtable with table = new_table } in
    VDomTable.set t.env.dom dom domtable;
    Explanation.add_pexp_dom t.env.trail pexp dom ~cl ~cl0;
    Wait.wakeup_events_bag Events.Wait.translate_dom t events (cl,dom)

  let set_dom_premerge_pending (type a) t (dom : a Dom.t)
      ~from:cl0' cl0 (new_v:a) =
    Debug.incr stats_set_dom;
    let cl' = find t cl0' in
    let cl   = find t cl0 in
    let domtable = (get_table_dom t.env dom) in
    let events = Cl.M.find_opt cl domtable.events in
    let new_table = Cl.M.add cl new_v domtable.table in
    let domtable = { domtable with table = new_table } in
    VDomTable.set t.env.dom dom domtable;
    Explanation.add_pexp_dom_premerge t.env.trail dom
      ~clfrom:cl' ~clfrom0:cl0' ~clto:cl;
    Wait.wakeup_events_bag Events.Wait.translate_dom t events (cl0,dom)


(*
  merge:
  1) choose the representative between cl1 and cl2
  2) "Merge" the semantical term and create new pending merge if the resulting
     sematical value already exists. Add pending event for the modification of
     the representative
  3) Merge the dom and add the pending event
*)

  let choose_repr a b = Shuffle.shuffle2 (a,b)

  (** TODO rename other_cl repr_cl *)
  let merge_dom_pending (type a) t pexp (dom : a Dom.t) other_cl0 repr_cl0 inv =
    let other_cl = find t other_cl0 in
    let repr_cl  = find t repr_cl0  in
    let domtable = (get_table_dom t.env dom) in
    let old_other_s = Cl.M.find_opt other_cl domtable.table in
    let old_repr_s = Cl.M.find_opt repr_cl  domtable.table in
    let module Dom = (val (VDom.get_dom dom)) in
    Debug.dprintf12 debug_few
      "[Solver] @[merge dom (%a(%a),%a)@ and (%a(%a),%a)@]"
      Cl.pp other_cl Cl.pp other_cl0
      (Pp.option Dom.pp) old_other_s
      Cl.pp repr_cl Cl.pp repr_cl0
      (Pp.option Dom.pp) old_repr_s;
    match old_other_s, old_repr_s with
    | None, None   -> ()
    | _ ->
      Dom.merge t pexp
        (old_other_s,other_cl0)
        (old_repr_s,repr_cl0)
        inv


  let merge_dom ?(dry_run=false) t pexp other_cl0 repr_cl0 inv =
    let other_cl = find t other_cl0 in
    let repr_cl  = find t repr_cl0  in
    let dom_not_done = ref false in
    let iteri (type a) (dom : a Dom.t) (domtable : a domtable) =
      let other_s = Cl.M.find_opt other_cl domtable.table in
      let repr_s  = Cl.M.find_opt repr_cl  domtable.table in
    let module Dom = (val (VDom.get_dom dom)) in
      if not (Dom.merged other_s repr_s)
      then begin
        dom_not_done := true;
        if not dry_run then
          Queue.push
            (SetMergeDomCl(pexp,dom,other_cl0,repr_cl0,inv)) t.todo_merge_dom
      end
    in
    VDomTable.iter_initializedi {VDomTable.iteri} t.env.dom;
    !dom_not_done

  let finalize_merge t _pexp other_cl0 repr_cl0 inv =
    let other_cl0,repr_cl0 =
      if inv
      then repr_cl0, other_cl0
      else other_cl0, repr_cl0 in
    let other_cl = find t other_cl0 in
    let repr_cl  = find t repr_cl0  in
    Debug.dprintf8 debug_few "[Solver.few] merge %a(%a) -> %a(%a)"
      Cl.pp other_cl Cl.pp other_cl0
      Cl.pp repr_cl Cl.pp repr_cl0;
    t.env.repr <- Cl.M.add other_cl repr_cl t.env.repr;
    add_merge_dom_all t.env.trail ~inv ~other_cl ~other_cl0 ~repr_cl ~repr_cl0;
    let event, other_event = Cl.M.find_remove other_cl t.env.event in

    (** move cl events *)
    begin match other_event with
      | None -> ()
      | Some other_event ->
        t.env.event <-
          Cl.M.add_change (fun x -> x) Bag.concat repr_cl other_event
            event
    end;

    (** move dom events  *)
    let iteri (type a) (dom : a Dom.t) (domtable: a domtable) =
      match Cl.M.find_opt other_cl domtable.events with
      | None -> ()
      | Some other_events ->
        let new_events =
          Cl.M.add_change (fun x -> x) Bag.concat repr_cl other_events
            domtable.events in
        let domtable = { domtable with events = new_events } in
        VDomTable.set t.env.dom dom domtable
    in
    VDomTable.iter_initializedi {VDomTable.iteri} t.env.dom;

    (** wakeup the daemons *)
    Wait.wakeup_events_bag
      Events.Wait.translate_change t other_event other_cl

  let finalize_merge_pending t pexp other_cl0 repr_cl0 inv  =
    let dom_not_done = merge_dom t pexp other_cl0 repr_cl0 inv in
    if dom_not_done
    then begin
      Debug.dprintf4 debug "[Solver] @[merge %a %a dom not done@]"
        Cl.pp other_cl0 Cl.pp repr_cl0;
      t.todo_delayed_merge <- Some (pexp,other_cl0,repr_cl0,inv)
    end
    else
      finalize_merge t pexp other_cl0 repr_cl0 inv

  (** merge two pending actions *)
  let merge_pending t pexp cl1_0 cl2_0 =
    let cl1 = find t cl1_0 in
    let cl2 = find t cl2_0 in
    if not (Cl.equal cl1 cl2) then begin
      let ((other_cl0,other_cl),(repr_cl0,repr_cl)) =
        choose_repr (cl1_0,cl1) (cl2_0,cl2) in
      let inv = not (Cl.equal cl1_0 other_cl0) in
      add_pexp_cl t.env.trail pexp
        ~inv ~other_cl ~other_cl0 ~repr_cl ~repr_cl0;
      finalize_merge_pending t pexp cl1_0 cl2_0 inv
    end

  let merge t pexp cl1_0 cl2_0 =
    assert (is_current_env t);
    if not (Cl.equal
              (find t cl1_0)
              (find t cl2_0)) then
      add_pending_merge t pexp cl1_0 cl2_0

  let set_sem  d pexp cl clsem =
    Debug.dprintf4 debug "[Solver] @[add_pending_set_sem for %a and %a@]"
      Cl.pp cl ClSem.pp clsem;
    assert (d.env.current_delayed == d);
    assert (is_registered d cl);
    set_sem_pending d pexp cl clsem
  let set_clvalue  d pexp cl clvalue =
    Debug.dprintf4 debug "[Solver] @[add_pending_set_clvalue for %a and %a@]"
      Cl.pp cl ClValue.pp clvalue;
    assert (d.env.current_delayed == d);
    assert (is_registered d cl);
    set_value_pending d pexp cl clvalue
  let set_value (type a)  d pexp (value : a value) cl v =
    Debug.dprintf4 debug_few
      "[Solver] @[set_dom for %a with %a@]"
      Cl.pp cl (print_value value) v;
    let clvalue = ClValue.index value v (Cl.ty cl) in
    set_value_pending d pexp cl clvalue
  let set_dom d pexp dom cl v =
    Debug.dprintf4 debug_few
      "[Solver] @[set_dom for %a with %a@]"
      Cl.pp cl (print_dom dom) v;
    assert (d.env.current_delayed == d);
    assert (is_registered d cl);
    set_dom_pending d pexp dom cl (Some v)
  let set_dom_premerge d dom cl v =
    Debug.dprintf4 debug
      "[Solver] @[set_dom_premerge for %a with %a@]"
      Cl.pp cl (print_dom dom) v;
    assert (d.env.current_delayed == d);
    assert (is_registered d cl);
    let cl' = match d.todo_delayed_merge with
    | Some(_,cl1,cl2,_) when Cl.equal cl1 cl -> cl2
    | Some(_,cl1,cl2,_) when Cl.equal cl2 cl -> cl1
    | _ -> raise (BrokenInvariant(
        "set_dom_premerge should be used only on the \
         classes currently merged")) in
    set_dom_premerge_pending d dom ~from:cl' cl v
  let unset_dom d pexp dom cl =
    Debug.dprintf2 debug
      "[Solver] @[unset_dom for %a@]"
      Cl.pp cl;
    assert (d.env.current_delayed == d);
    assert (is_registered d cl);
    set_dom_pending d pexp dom cl None


  let rec do_pending_daemon delayed (Events.Wait.DaemonKey (dem,runable)) =
    let module Dem = (val Wait.get_dem dem) in
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
        Debug.dprintf0 debug "[Solver] @[do_pending RunDem immediate@]";
        do_pending_daemon t att;
        do_pending t
    else if not (Queue.is_empty t.todo_merge_dom) then
      match Queue.pop t.todo_merge_dom with
      | SetMergeDomCl(pexp,dom,cl1,cl2,inv) ->
        Debug.dprintf6 debug "[Solver] @[do_pending SetDomCl %a %a %a@]"
          Dom.pp dom Cl.pp cl1 Cl.pp cl2;
        merge_dom_pending t pexp dom cl1 cl2 inv;
        do_pending t
    else match t.todo_delayed_merge with
      | Some(pexp,other_cl,repr_cl,inv) ->
        t.todo_delayed_merge <- None;
        assert (not (merge_dom ~dry_run:true t pexp other_cl repr_cl inv));
        (** understand why that happend.
            Is it really needed to do a fixpoint? *)
        finalize_merge_pending t pexp other_cl repr_cl inv;
        do_pending t
    | None ->
      if not (Queue.is_empty t.todo_merge) then
      match Queue.pop t.todo_merge with
      | Merge (pexp,cl1,cl2) ->
        Debug.dprintf4 debug "[Solver] @[do_pending Merge %a %a@]"
          Cl.pp cl1 Cl.pp cl2;
        merge_pending t pexp cl1 cl2;
        do_pending t
    else if not (Queue.is_empty t.todo_ext_action) then
      (begin match Queue.pop t.todo_ext_action with
      (* | ExtSetDom (pexp,dom,cl,v) -> *)
      (*   Queue.push (SetDom(pexp,dom,cl,v)) t.todo_dom *)
      (* | ExtSetMergeDom (pexp,dom,cl,v) -> *)
      (*   Queue.push (SetMergeDomVal(pexp,dom,cl,v)) t.todo_merge_dom *)
      (* | ExtSetSem (pexp,sem,cl,v) -> *)
      (*   Queue.push (SetSem(pexp,sem,cl,v)) t.todo_sem *)
      (* | ExtMerge (pexp,cl1,cl2) -> *)
      (*   Queue.push (Merge(pexp,cl1,cl2)) t.todo_merge *)
      | ExtDem att ->
        Debug.dprintf0 debug "[Solver] @[do_pending RunDem@]";
        let store_ext_action = Queue.create () in
        Queue.transfer t.todo_ext_action store_ext_action;
        do_pending_daemon t att;
        Queue.transfer store_ext_action t.todo_ext_action;
       end;
       do_pending t)
    else
      Debug.dprintf0 debug "[Solver] Nothing to do"

  and flush d =
    assert (d.env.current_delayed == d);
    Debug.dprintf0 debug "[Solver] @[flush delayed@]";
    try
      if not (Queue.is_empty d.todo_ext_action) then
        let saved_ext_action = Queue.create () in
        Queue.transfer d.todo_ext_action saved_ext_action;
        do_pending d;
        Queue.transfer saved_ext_action d.todo_ext_action;
      else
        do_pending d;
      assert (nothing_todo d);
      Debug.dprintf0 debug "[Solver] @[flush delayed end@]"
    with e when Debug.test_flag debug &&
                not (Debug.test_flag Debug.stack_trace) ->
      raise e

  let register_decision t chogen =
    t.sched_decision chogen

  let mk_pexp t ?age ?tags kexp exp = mk_pexp ?age ?tags t.env.trail kexp exp
  let current_age t = Explanation.current_age t.env.trail

  let contradiction d pexp =
    d.env.current_delayed <- unsat_delayed;
    raise (Contradiction pexp)

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

let is_equal t cl1 cl2 =
  assert (t.current_delayed == dumb_delayed);
  let cl1,cl2 = Shuffle.shuffle2 (cl1,cl2) in
  Debug.dprintf4 debug "[Solver] @[is_equal %a %a@]"
    Cl.pp cl1 Cl.pp cl2;
  draw_graph t;
  is_equal t cl1 cl2

let find t cl =
  assert (t.current_delayed == dumb_delayed);
  find t cl

let get_dom t dom cl =
  assert (t.current_delayed == dumb_delayed);
  get_dom t dom cl

let get_value t value cl =
  assert (t.current_delayed == dumb_delayed);
  get_value t value cl

let get_env t env =
  assert (t.current_delayed == dumb_delayed);
  get_env t env

let set_env t env v =
  assert (t.current_delayed == dumb_delayed);
  set_env t env v

let is_repr t cl =
  assert (t.current_delayed == dumb_delayed);
  is_repr t cl

let find_def t cl =
  assert (t.current_delayed == dumb_delayed);
  find_def t cl

let get_trail t =
  assert (t.current_delayed == dumb_delayed ||
          t.current_delayed == unsat_delayed);
  t.trail


let new_dec t =
  assert (t.current_delayed == dumb_delayed);
  let t' = new_handle t in
  Explanation.new_dec
    {dom_before_last_dec = (fun dom cl -> get_dom t' dom cl)}
    t.trail

let current_age (t:t) = Explanation.current_age t.trail
let current_nbdec (t:t) = Explanation.nbdec t.trail

let get_direct_dom t dom cl =
  assert (t.current_delayed == dumb_delayed ||
          t.current_delayed == unsat_delayed);
  get_direct_dom t dom cl


module type Getter = sig
  type t

  val is_equal      : t -> Cl.t -> Cl.t -> bool
  val find_def  : t -> Cl.t -> Cl.t
  val get_dom   : t -> 'a Dom.t -> Cl.t -> 'a option
    (** dom of the class *)
  val get_value   : t -> 'a value -> Cl.t -> 'a option
    (** value of the class *)

  (** {4 The classes must have been marked has registered} *)

  val find      : t -> Cl.t -> Cl.t
  val is_repr      : t -> Cl.t -> bool

  val is_registered : t -> Cl.t -> bool

  val get_env : t -> 'a Env.t -> 'a
  val set_env: t -> 'a Env.t -> 'a -> unit

end

module type Ro = sig
  include Getter

  (** {3 Immediate information} *)
  val register : t -> Cl.t -> unit

  val is_current_env: t -> bool

end

type d = Delayed.t

module Ro : Ro with type t = Delayed.t = Delayed

let check_initialization () =
  VDom.well_initialized () && Wait.well_initialized ()

let () = Exn_printer.register (fun fmt exn ->
    match exn with
    | UninitializedEnv env ->
      Format.fprintf fmt "The environnement of %a is not initialized."
        Env.K.pp env
    | exn -> raise exn
  )
