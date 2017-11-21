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

module VEnv = Env.MkVector(struct type ('a,'b) t = 'a Pp.pp end)
let defined_env = VEnv.create 8
let print_env k =
  assert (if VEnv.is_uninitialized defined_env k
    then raise UnregisteredKey else true);
  VEnv.get defined_env k

let register_env pp env =
  VEnv.inc_size env defined_env;
  assert (if not (VEnv.is_uninitialized defined_env env)
          then raise AlreadyRegisteredKey else true);
  VEnv.set defined_env env pp


module Events = struct

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
  end


  module Fired = struct
    type 'b event =
      (** the domain dom of the class change *)
    | EventDom    : Cl.t * 'a dom  *      'b -> 'b event
      (** the value of the class has been set *)
    | EventValue    : Cl.t * 'a value  *  'b -> 'b event
      (** a new semantical term 'a point to this class (not complete) *)
    | EventSem    : Cl.t * 'a sem  * 'a * 'b -> 'b event
      (** we want to register a class *)
    | EventReg    : Cl.t *                'b -> 'b event
      (** we want to register this class *)
    | EventRegCl  : Cl.t *                'b -> 'b event
      (** This class is not the representant of its eq-class anymore *)
    | EventChange : Cl.t *                'b -> 'b event
    (** a new semantical term 'a appear *)
    | EventRegSem : ClSem.t * 'b -> 'b event
    (** a new value 'a appear *)
    | EventRegValue : ClValue.t * 'b -> 'b event

    type 'a translate = { translate : 'd. 'a -> 'd -> 'd event}

    let translate_dom =
      {translate = fun (cl,dom) data -> EventDom(cl,dom,data)}
    let translate_value =
      {translate = fun (cl,value) data -> EventValue(cl,value,data)}
    (* let translate_sem = *)
    (*   {translate = fun (cl,sem,s) data -> EventSem(cl,sem,s,data)} *)
    let translate_reg =
      {translate = fun cl data -> EventReg(cl,data)}
    let translate_regcl =
      {translate = fun cl data -> EventRegCl(cl,data)}
    let translate_change =
      {translate = fun cl data -> EventChange(cl,data)}
    let translate_regsem =
      {translate = fun clsem data -> EventRegSem(clsem,data)}
    let translate_regvalue =
      {translate = fun clval data -> EventRegValue(clval,data)}

    let pp fmt = function
      | EventDom      (cl, dom, _) ->
        Format.fprintf fmt "dom:%a of %a" Dom.pp dom Cl.pp cl
      | EventValue    (cl, value, _) ->
        Format.fprintf fmt "value:%a of %a" Value.pp value Cl.pp cl
      | EventSem      (cl, sem, v, _) ->
        Format.fprintf fmt "sem:%a of %a with %a"
          Sem.pp sem Cl.pp cl (print_sem sem) v
      | EventReg      (cl, _)    ->
        Format.fprintf fmt "any registration of %a" Cl.pp cl
      | EventRegCl    (cl, _)    ->
        Format.fprintf fmt "registration of %a" Cl.pp cl
      | EventChange   (cl, _)    ->
        Format.fprintf fmt "changecl of %a" Cl.pp cl
      | EventRegSem (clsem, _) ->
        let cl = Only_for_solver.cl_of_clsem clsem in
        begin match Only_for_solver.sem_of_cl clsem with
        | Only_for_solver.Sem(sem,v) ->
          Format.fprintf fmt "registration of sem:%a of %a with %a"
            Sem.pp sem Cl.pp cl (print_sem sem) v
        end
      | EventRegValue (clvalue, _) ->
        let cl = Only_for_solver.cl_of_clvalue clvalue in
        begin match Only_for_solver.value_of_cl clvalue with
        | Only_for_solver.Value(value,v) ->
          Format.fprintf fmt "registration of value:%a of %a with %a"
            Value.pp value Cl.pp cl (print_value value) v
        end

    let get_data = function
      | EventDom      (_, _ , d)   -> d
      | EventValue    (_, _ , d)   -> d
      | EventSem      (_, _, _, d) -> d
      | EventReg    (_, d)       -> d
      | EventRegCl  (_, d)       -> d
      | EventChange   (_, d)       -> d
      | EventRegSem (_, d) -> d
      | EventRegValue (_,d) -> d


    type 'b t = 'b event list
  end

end

module type Dom' = sig
  type delayed
  type t

  val merged: t option -> t option -> bool
  val merge: delayed ->
    Explanation.pexp -> t option * Cl.t -> t option * Cl.t ->
    bool ->
    unit
  val pp: Format.formatter  -> t  -> unit
  val key: t dom
end


type _ enqueue =
| EnqRun: 'r -> 'r enqueue
| EnqAlready: _ enqueue
| EnqRedirected: ('e,'r) dem * 'e -> _ enqueue
| EnqStopped: _ enqueue

module type Dem' = sig
  type delayed

  type runable
  val print_runable: runable Pp.pp
  val run: delayed -> runable -> runable option

  type event
  val print_event: event Pp.pp
  val enqueue: delayed -> event Events.Fired.event -> runable enqueue

  val key: (event,runable) dem
  val immediate: bool

end

module DecTag = DInt

module type DomTable' = sig
  type delayed
  module D : Dom' with type delayed := delayed
  val table : D.t Cl.M.t
  val events : Events.Wait.t Bag.t Cl.M.t
end

type semtable = Events.Wait.t list

module VDomTable = Dom.MkVector
  (struct type ('a,'delayed) t =
            (module DomTable' with type D.t = 'a and type delayed = 'delayed)
   end)

module VSemTable = Sem.Vector

module type ValueTable = sig
  module D : Typedef.Value
  val table : D.t Cl.M.t
  val events : Events.Wait.t Bag.t Cl.M.t
  val reg_events : Events.Wait.t list
end

module VValueTable = Value.MkVector
  (struct type ('a,'unit) t =
            (module ValueTable with type D.t = 'a)
   end)

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

and daemon_key =
| DaemonKey: ('k,'runable) dem * 'runable -> daemon_key

(** delayed_t is used *)
and delayed_t = {
  env : t;
  todo_immediate_dem : action_immediate_dem Queue.t;
  todo_merge_dom : action_merge_dom Queue.t;
  mutable todo_delayed_merge : (pexp * Cl.t * Cl.t * bool) option;
  todo_merge : action_merge Queue.t;
  todo_ext_action : action_ext Queue.t;
  sched_daemon : daemon_key -> unit;
  sched_decision : chogen -> unit;
}

and action_immediate_dem =
| RunDem : daemon_key -> action_immediate_dem

and action_merge_dom =
| SetMergeDomCl  :
    pexp * 'a dom * Cl.t * Cl.t * bool -> action_merge_dom

and action_merge =
| Merge of pexp * Cl.t * Cl.t

and action_ext =
(* | ExtSetDom      : pexp * 'a dom * Cl.t * 'a        -> action_ext *)
(* | ExtSetMergeDom : pexp * 'a dom * Cl.t * 'a option -> action_ext *)
(* | ExtSetSem      : pexp * 'a sem * Cl.t * 'a        -> action_ext *)
(* | ExtMerge       : pexp * Cl.t * Cl.t -> action_ext *)
| ExtDem         : daemon_key  -> action_ext

end

include Def

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

let new_handler t =
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

(** {2 Dom and Sem} *)
module type Dom = Dom' with type delayed := delayed_t
module type Dem = Dem' with type delayed := delayed_t

module VDom = Dom.MkVector
  (struct type ('a,'unedeed) t =
            (module Dom with type t = 'a)
   end)

module VDem = Dem.MkVector
  (struct type ('k,'d,'unedeed) t =
    (module Dem with type event = 'k and type runable = 'd) end)

let defined_dom : unit VDom.t = VDom.create 8
let defined_dem : unit VDem.t = VDem.create 8

module RegisterDom (D:Dom) = struct

  let () =
    VDom.inc_size D.key defined_dom;
    assert (if not (VDom.is_uninitialized defined_dom D.key)
      then raise AlreadyRegisteredKey else true);
    let dom = (module D: Dom with type t = D.t) in
    VDom.set defined_dom D.key dom

end


module RegisterDem (D:Dem) = struct

  let () =
    VDem.inc_size D.key defined_dem;
    assert (if not (VDem.is_uninitialized defined_dem D.key)
      then raise AlreadyRegisteredKey else true);
    let dem =
      (module D: Dem with type event = D.event and type runable = D.runable) in
    VDem.set defined_dem D.key dem

end

let get_dom k =
  assert (if VDom.is_uninitialized defined_dom k
    then raise UnregisteredKey else true);
  VDom.get defined_dom k

let get_dem k =
  assert (if VDem.is_uninitialized defined_dem k
    then raise UnregisteredKey else true);
  VDem.get defined_dem k

let print_dom (type a) (k : a dom) fmt s =
  let dom = get_dom k in
  let module D = (val dom : Dom with type t = a) in
  D.pp fmt s

let print_dom_opt k fmt = function
  | None -> Format.pp_print_string fmt "N"
  | Some s -> print_dom k fmt s

let print_dem_event (type k) (type d) (k : (k,d) dem) fmt s =
  let module S = (val get_dem k) in
  S.print_event fmt s

let () = Print.pdem_event.Print.pdem_event <- print_dem_event

let print_dem_runable (type k) (type d) (k : (k,d) dem) fmt s =
  let module S = (val get_dem k) in
  S.print_runable fmt s

let () = Print.pdem_runable.Print.pdem_runable <- print_dem_runable

(** {2 Dom Sem continued} *)

module type DomTable = DomTable' with type delayed = delayed_t

let get_table_dom : t -> 'a dom -> (module DomTable with type D.t = 'a)
  = fun (type a) t k ->
  assert (if VDom.is_uninitialized defined_dom k
    then raise UnregisteredKey else true);
  VDomTable.inc_size k t.dom;
  if VDomTable.is_uninitialized t.dom k then
    let dom = get_dom k in
    let module DomTable = struct
      type delayed = delayed_t
      module D = (val dom : Dom with type t = a)
      let table = Cl.M.empty
      let events = Cl.M.empty
    end in
    (module DomTable : DomTable with type D.t = a)
  else
    (module (val VDomTable.get t.dom k
        : DomTable' with type D.t = a and type delayed = delayed_t)
        : DomTable with type D.t = a)

let get_table_sem : t -> 'a sem -> semtable = fun t k ->
  assert (if sem_uninitialized k then raise UnregisteredKey else true);
  VSemTable.inc_size k t.sem;
  if VSemTable.is_uninitialized t.sem k
  then begin Sem.Vector.set t.sem k []; [] end
  else Sem.Vector.get t.sem k

let get_table_value : t -> 'a value -> (module ValueTable with type D.t = 'a)
  = fun (type a) t k ->
  assert (if value_uninitialized k
    then raise UnregisteredKey else true);
  VValueTable.inc_size k t.value;
  if VValueTable.is_uninitialized t.value k then
    let value = get_value k in
    let module ValueTable = struct
      module D = (val value : Value with type t = a)
      let table = Cl.M.empty
      let events = Cl.M.empty
      let reg_events = []
    end in
    (module ValueTable : ValueTable with type D.t = a)
  else
    (module (val VValueTable.get t.value k
        : ValueTable with type D.t = a)
        : ValueTable with type D.t = a)


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

let get_direct_dom (type a) t (dom : a dom) cl =
  let module DomTable =
    (val (get_table_dom t dom) : DomTable with type D.t = a) in
  Cl.M.find_opt cl DomTable.table

let get_dom t dom cl =
  let cl = find_def t cl in
  get_direct_dom t dom cl

let get_direct_value (type a) t (value : a value) cl =
  let module ValueTable =
    (val (get_table_value t value) : ValueTable with type D.t = a) in
  Cl.M.find_opt cl ValueTable.table

let get_value t value cl =
  let cl = find_def t cl in
  get_direct_value t value cl

(** {2 For debugging and display} *)
let _print_env fmt t =
  let printd (type a) _ fmt domtable =
    let module DomTable =
      (val domtable : DomTable' with type delayed = delayed_t
                                 and type D.t = a) in
    Format.fprintf fmt "%a:@[%a@]" Dom.pp DomTable.D.key
      (Pp.iter2 Cl.M.iter Pp.newline Pp.colon
         Cl.pp (Bag.pp Pp.comma Events.Wait.pp))
      DomTable.events
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
      let iter_dom (type a) _ fmt dom =
        let module Dom =
              (val dom : DomTable' with type delayed = delayed_t
                                   and type D.t = a) in
        try
          let s   = Cl.M.find cl Dom.table in
          Format.fprintf fmt "| {%a | %s}"
            Typedef.Dom.pp Dom.D.key (escape_for_dot Dom.D.pp s);
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


  let is_registered t cl = Cl.M.mem cl t.env.repr

  let new_pending_daemon (type k) (type d) t (dem:(k,d) dem) runable =
    let module Dem = (val get_dem dem) in
    let daemonkey = DaemonKey(dem, runable) in
    if Dem.immediate
    then Queue.push (RunDem daemonkey) t.todo_immediate_dem
    else t.sched_daemon daemonkey

  let wakeup_event translate t info wevent =
    match wevent with
    | Events.Wait.Event (dem,event) ->
      let rec f : type event r. t -> (event,r) dem -> event -> unit =
        fun t dem event ->
          let module Dem = (val get_dem dem) in
          let event = translate.Events.Fired.translate info event in
          match Dem.enqueue t event with
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

  let set_value_direct (type a) t pexp (value : a value) cl0 new_v =
    Debug.incr stats_set_value;
    let cl = find t cl0 in
    let module ValueTable = (val (get_table_value t.env value)) in
    let events = Cl.M.find_opt cl ValueTable.events in
    let new_table = Cl.M.add cl new_v ValueTable.table in
    let module ValueTable' = struct
      include ValueTable
      let table = new_table
    end in
    VValueTable.set t.env.value value (module ValueTable');
    Explanation.add_pexp_value t.env.trail pexp value ~cl ~cl0;
    wakeup_events_bag Events.Fired.translate_value t events (cl,value)

  let merge_values t pexp cl0 cl0' =
    let cl  = find t cl0 in
    let cl' = find t cl0'  in
    let iteri (type a) value valuetable =
      let module ValueTable =
        (val valuetable : ValueTable with type D.t = a) in
      let old_s = Cl.M.find_opt cl ValueTable.table in
      let old_s'  = Cl.M.find_opt cl'  ValueTable.table in
      Debug.dprintf12 debug_few
        "[Solver] @[merge value (%a(%a),%a)@ and (%a(%a),%a)@]"
        Cl.pp cl Cl.pp cl0
        (Pp.option ValueTable.D.pp) old_s
        Cl.pp cl' Cl.pp cl0'
        (Pp.option ValueTable.D.pp) old_s';
      match old_s, old_s' with
      | None, None   -> ()
      | Some v, None ->
        set_value_direct t pexp value cl0' v
      | None, Some v' ->
        set_value_direct t pexp value cl0  v'
      | Some v, Some v' ->
        if ValueTable.D.equal v v'
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


  let attach_dom (type a) t cl (dom : a dom) dem event =
    let cl = find_def t cl in
    let event = Events.Wait.Event (dem,event) in
    let module DomTable = (val (get_table_dom t.env dom)) in
    let module DomTable' = struct
      (** remove the old domantical value
              replace it by the new one *)
      include DomTable
      let events = Cl.M.add_change Bag.elt Bag.add cl event events
    end in
    VDomTable.set t.env.dom dom (module DomTable')

  let attach_value (type a) t cl (value : a value) dem event =
    let cl = find_def t cl in
    let event = Events.Wait.Event (dem,event) in
    let module ValueTable = (val (get_table_value t.env value)) in
    let module ValueTable' = struct
      include ValueTable
      let events = Cl.M.add_change Bag.elt Bag.add cl event events
    end in
    VValueTable.set t.env.value value (module ValueTable)

  let attach_cl t cl dem event =
    let cl = find_def t cl in
    let event = Events.Wait.Event (dem,event) in
    t.env.event <- Cl.M.add_change Bag.elt Bag.add cl event t.env.event

  let attach_reg_cl t cl dem event =
    let event = Events.Wait.Event (dem,event) in
    begin try
        let cl = find t cl in
        (** already registered *)
        wakeup_events_list Events.Fired.translate_regcl t (Some [event]) cl
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
            | Typedef.Eq, Typedef.Eq -> (event:k)
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
            | Typedef.Eq, Typedef.Eq -> (event:k)
        )
       (Cl.M.find_def Bag.empty cl d.env.event)


(** *)


  let check_no_dom t cl =
    let foldi (type a) acc _dom domtable =
      acc &&
      let module DomTable =
        (val domtable : DomTable' with type D.t = a
                                   and type delayed = delayed_t) in
      not (Cl.M.mem cl DomTable.table)
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
      wakeup_events_list Events.Fired.translate_regcl t cl_events cl;
      (** reg *)
      wakeup_events_list Events.Fired.translate_reg
        t (Some t.env.event_any_reg) cl;
      (** reg_sem *)
      match Only_for_solver.open_cl cl with
      | Only_for_solver.Fresh -> ()
      | Only_for_solver.Fresh_to_reg(dem,event) ->
        wakeup_events_list Events.Fired.translate_regcl t
          (Some [Events.Wait.Event(dem,event)])
          cl;
      | Only_for_solver.Sem clsem ->
        begin match Only_for_solver.sem_of_cl clsem with
        | Only_for_solver.Sem(sem,_) ->
          let reg_events = get_table_sem t.env sem in
          wakeup_events_list Events.Fired.translate_regsem
            t (Some reg_events) (clsem)
        end
      | Only_for_solver.Value clvalue ->
        begin match Only_for_solver.value_of_cl clvalue with
        | Only_for_solver.Value(value,v) ->
          let module V = (val get_table_value t.env value) in
          let reg_events = V.reg_events in
          wakeup_events_list Events.Fired.translate_regvalue
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
        wakeup_events_bag Events.Fired.translate_change t other_event cl0';
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

  let set_dom_pending (type a) t pexp (dom : a dom) cl0 new_v =
    Debug.incr stats_set_dom;
    let cl = find t cl0 in
    let module DomTable = (val (get_table_dom t.env dom)) in
    let events = Cl.M.find_opt cl DomTable.events in
    let new_table = Cl.M.add_opt cl new_v DomTable.table in
    let module DomTable' = struct
      include DomTable
      let table = new_table
    end in
    VDomTable.set t.env.dom dom (module DomTable');
    Explanation.add_pexp_dom t.env.trail pexp dom ~cl ~cl0;
    wakeup_events_bag Events.Fired.translate_dom t events (cl,dom)

  let set_dom_premerge_pending (type a) t (dom : a dom)
      ~from:cl0' cl0 (new_v:a) =
    Debug.incr stats_set_dom;
    let cl' = find t cl0' in
    let cl   = find t cl0 in
    let module DomTable = (val (get_table_dom t.env dom)) in
    let events = Cl.M.find_opt cl DomTable.events in
    let new_table = Cl.M.add cl new_v DomTable.table in
    let module DomTable' = struct
      include DomTable
      let table = new_table
    end in
    VDomTable.set t.env.dom dom (module DomTable');
    Explanation.add_pexp_dom_premerge t.env.trail dom
      ~clfrom:cl' ~clfrom0:cl0' ~clto:cl;
    wakeup_events_bag Events.Fired.translate_dom t events (cl0,dom)


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
  let merge_dom_pending (type a) t pexp (dom : a dom) other_cl0 repr_cl0 inv =
    let other_cl = find t other_cl0 in
    let repr_cl  = find t repr_cl0  in
    let module DomTable = (val (get_table_dom t.env dom)) in
    let old_other_s = Cl.M.find_opt other_cl DomTable.table in
    let old_repr_s = Cl.M.find_opt repr_cl  DomTable.table in
    Debug.dprintf12 debug_few
      "[Solver] @[merge dom (%a(%a),%a)@ and (%a(%a),%a)@]"
      Cl.pp other_cl Cl.pp other_cl0
      (Pp.option DomTable.D.pp) old_other_s
      Cl.pp repr_cl Cl.pp repr_cl0
      (Pp.option DomTable.D.pp) old_repr_s;
    match old_other_s, old_repr_s with
    | None, None   -> ()
    | _ ->
      DomTable.D.merge t pexp
        (old_other_s,other_cl0)
        (old_repr_s,repr_cl0)
        inv


  let merge_dom ?(dry_run=false) t pexp other_cl0 repr_cl0 inv =
    let other_cl = find t other_cl0 in
    let repr_cl  = find t repr_cl0  in
    let dom_not_done = ref false in
    let iteri (type a) dom domtable =
      let module DomTable =
        (val domtable : DomTable' with type delayed = delayed_t
                                   and type D.t = a) in
      let other_s = Cl.M.find_opt other_cl DomTable.table in
      let repr_s  = Cl.M.find_opt repr_cl  DomTable.table in
      if not (DomTable.D.merged other_s repr_s)
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
    let iter (type a) domtable =
      let module DomTable =
        (val domtable : DomTable' with type delayed = delayed_t
                                   and type D.t = a) in
      match Cl.M.find_opt other_cl DomTable.events with
      | None -> ()
      | Some other_events ->
        let new_events =
          Cl.M.add_change (fun x -> x) Bag.concat repr_cl other_events
            DomTable.events in
        let module DomTable' = struct
          include DomTable
          let events = new_events
        end in
        VDomTable.set t.env.dom DomTable.D.key (module DomTable')
    in
    VDomTable.iter_initialized {VDomTable.iter} t.env.dom;

    (** wakeup the daemons *)
    wakeup_events_bag
      Events.Fired.translate_change t other_event other_cl

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
  let set_value  d pexp cl clvalue =
    Debug.dprintf4 debug "[Solver] @[add_pending_set_value for %a and %a@]"
      Cl.pp cl ClValue.pp clvalue;
    assert (d.env.current_delayed == d);
    assert (is_registered d cl);
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


  let rec do_pending_daemon delayed (DaemonKey (dem,runable)) =
    let module Dem = (val get_dem dem) in
    match Dem.run delayed runable with
    | None -> ()
    | Some runable -> new_pending_daemon delayed dem runable

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

  let get_env : type a. t -> a env -> a
    = fun t k ->
      assert (if VEnv.is_uninitialized defined_env k
              then raise UnregisteredKey else true);
      Env.VectorH.inc_size k t.env.envs;
      if Env.VectorH.is_uninitialized t.env.envs k then
        raise (UninitializedEnv (k :> Env.K.t))
      else
        Env.VectorH.get t.env.envs k

  let set_env : type a. t -> a env -> a -> unit
    = fun t k ->
      assert (if VEnv.is_uninitialized defined_env k
              then raise UnregisteredKey else true);
      Env.VectorH.inc_size k t.env.envs;
      Env.VectorH.set t.env.envs k

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

let get_trail t =
  assert (t.current_delayed == dumb_delayed ||
          t.current_delayed == unsat_delayed);
  t.trail

let new_dec t =
  assert (t.current_delayed == dumb_delayed);
  let t' = new_handler t in
  Explanation.new_dec
    {dom_before_last_dec = (fun dom cl -> get_dom t' dom cl)}
    t.trail

let current_age (t:t) = Explanation.current_age t.trail
let current_nbdec (t:t) = Explanation.nbdec t.trail

let get_direct_dom t dom cl =
  assert (t.current_delayed == dumb_delayed ||
          t.current_delayed == unsat_delayed);
  get_direct_dom t dom cl

module type Ro = sig
  type t
  (** {3 Immediate information} *)
  val register : t -> Cl.t -> unit
  (** Add a new class to register *)

  val is_equal      : t -> Cl.t -> Cl.t -> bool
  val find_def  : t -> Cl.t -> Cl.t

  (** {4 The classes must have been marked has registered} *)
  val get_dom   : t -> 'a dom -> Cl.t -> 'a option
    (** dom of the representative class *)

  val find      : t -> Cl.t -> Cl.t
  val is_repr      : t -> Cl.t -> bool

  val is_registered : t -> Cl.t -> bool

  val get_env : t -> 'a env -> 'a
  val set_env: t -> 'a env -> 'a -> unit


  (** Registered for events *)
  val attached_reg_cl:
    t -> Cl.t -> ('event,'d) dem -> 'event Enum.t
  val attached_cl:
    t -> Cl.t -> ('event,'d) dem -> 'event Enum.t

  val is_current_env: t -> bool

end

type d = Delayed.t

module Ro : Ro with type t = Delayed.t = Delayed

let check_initialization () =
  let well_initialized = ref true in

  Dom.iter {Dom.iter = fun dom ->
    if VDom.is_uninitialized defined_dom dom then begin
      Format.eprintf
        "[Warning] The domain %a is not registered" Dom.pp dom;
      well_initialized := false;
    end else begin
      Debug.dprintf2 debug "[Solver] @[domain %a initialized@]"
        Dom.pp dom;
    end};

  Dem.iter {Dem.iter = fun dem ->
    if VDem.is_uninitialized defined_dem dem then begin
      Format.eprintf
        "[Warning] The daemon %a is not registered" Dem.pp dem;
      well_initialized := false;
    end};

  !well_initialized

let () = Exn_printer.register (fun fmt exn ->
    match exn with
    | UninitializedEnv env ->
      Format.fprintf fmt "The environnement of %a is not initialized."
        Env.K.pp env
    | exn -> raise exn
  )
