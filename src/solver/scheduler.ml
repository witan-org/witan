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

open Witan_core

module S = Egraph

let debug = Debug.register_info_flag
  ~desc:"for the scheduler in the simple version"
  "sched_queue"

let debug_pushpop = Debug.register_info_flag
  ~desc:"for the scheduler push/pop"
  "sched_pushpop"

let debug_dotgui = Debug.register_flag
  ~desc:"print graph at interesting time (push/pop)"
  "sched_dotgui"


let var_decay = 1. /. 0.95

let stats_propa = Debug.register_stats_int ~name:"Scheduler.daemon" ~init:0
let stats_dec = Debug.register_stats_int ~name:"Scheduler.decision" ~init:0
let stats_con = Debug.register_stats_int ~name:"Scheduler.conflict" ~init:0

exception NeedStopDelayed

module Att = struct
  type t =
    | Daemon   of int * Events.Wait.daemon_key
    | Decision of int * Trail.chogen
  type prio = float
  type db = float Node.H.t

  let get_prio db n =
    Node.H.find_def db 0. n

  let le (x:t) (xp:float) (y:t) (yp:float) =
    match x, y with
    | Daemon (x,_)  , Daemon (y,_)   -> x <= y
    | Decision (x,_), Decision (y,_) ->
      if xp = yp then x <= y else xp >= yp (** min *)
    | Daemon _  , Decision _ -> true
    | Decision _, Daemon _   -> false
  let reprio db = function
    | Daemon _ -> 0.
    | Decision (_,Trail.GCho(n,_,_)) -> get_prio db n
end
exception Contradiction

module Prio = Leftistheap.Make(Att)

type pre =
  { pre_wakeup_daemons    : Prio.t;
    pre_prev_scheduler_state : pre option;
    pre_backtrack_point      : Context.bp;
    pre_age_dec : Trail.Age.t;
    pre_learnt : Conflict.Learnt.t Bag.t;
    pre_last_dec : Trail.chogen;
  }

type t =
  { mutable wakeup_daemons    : Prio.t;
    mutable prev_scheduler_state : pre option;
            solver_state      : S.Backtrackable.t;
    mutable delayed           : S.t option;
    mutable learnt : Conflict.Learnt.t Bag.t;
    (* global *)
    decprio : Att.db;
    var_inc  : float ref;
    context : Context.context;
  }
(** To treat in the reverse order *)

let get_t t = t.solver_state

let print_level fmt t =
  let nb_dec =
    Prio.fold (fun acc x _ -> match x with Att.Decision _ -> acc + 1 | _ -> acc)
      0 t.wakeup_daemons in
  Format.fprintf fmt "%a (level:%i, dec waiting:%i)"
    Trail.Age.pp (S.Backtrackable.current_age t.solver_state)
    (S.Backtrackable.current_nbdec t.solver_state) nb_dec

(* let new_handler t =
 *   if t.delayed <> None then raise NeedStopDelayed;
 *   {wakeup_daemons    = t.wakeup_daemons;
 *    prev_scheduler_state = t.prev_scheduler_state;
 *    solver_state      = S.Backtrackable.new_handle t.solver_state;
 *    learnt = t.learnt;
 *    delayed           = None;
 *    decprio = t.decprio;
 *    var_inc = t.var_inc
 *   } *)

let new_solver () =
  let context = Context.create () in
  { wakeup_daemons = Prio.empty;
    prev_scheduler_state = None;
    solver_state = S.Backtrackable.new_t (Context.creator context);
    context;
    learnt = Bag.empty;
    delayed    = None;
    decprio = Node.H.create 100;
    var_inc = ref 1.;
  }

let push t chogen =
  if Debug.test_flag debug_dotgui then
    S.Backtrackable.draw_graph ~force:true t.solver_state;
  Debug.dprintf0 debug_pushpop "[Scheduler] push";
  let age_dec = Trail.last_dec (S.Backtrackable.get_trail t.solver_state) in
  let prev =
    { pre_wakeup_daemons    = t.wakeup_daemons;
      pre_prev_scheduler_state = t.prev_scheduler_state;
      pre_backtrack_point      = Context.bp t.context;
      pre_learnt = t.learnt;
      pre_last_dec = chogen;
      pre_age_dec = age_dec;
    } in
  t.prev_scheduler_state <- Some prev;
  t.learnt <- Bag.empty;
  ignore (Context.push t.context)

let update_prio t chogen =
  Node.H.change (function
      | None -> Some (!(t.var_inc))
      | Some i -> Some (i +. (!(t.var_inc)))) t.decprio chogen

let new_delayed =
  let daemon_count = ref (-1) in
  let dec_count = ref (-1) in
  fun t ->
    let sched_daemon att =
      incr daemon_count;
      Debug.dprintf1 debug "[Scheduler] New possible daemon:%i"
        !daemon_count;
      t.wakeup_daemons <-
        Prio.insert t.decprio (Att.Daemon (!daemon_count,att))
          t.wakeup_daemons in
    let sched_decision dec =
      incr dec_count;
      Debug.dprintf1 debug "[Scheduler] New possible decisions prio:%i"
        !dec_count;
      t.wakeup_daemons <- Prio.insert t.decprio (Att.Decision (!dec_count,dec))
          t.wakeup_daemons in
    S.Backtrackable.new_delayed ~sched_daemon ~sched_decision t.solver_state

(*
  let rec apply_learnt llearnt t d =
    match llearnt with
    | [] -> d
    | a::l ->
      (** the first one should be the last conflict found so decide on it *)
      try
        let {Conflict.fin_dec = Trail.GCho(cho,k)} = a d in
        S.flush d;
        List.iter (fun f -> ignore (f d); S.flush d) l;
        run_until_dec t d;
        run_dec t d t.wakeup_daemons
          (fun d dec -> Conflict.make_decision d dec cho k)
      with S.Contradiction pexp ->
        Debug.dprintf0 debug "[Scheduler] Contradiction during apply learnt";
        conflict_analysis t pexp
*)

let rec apply_learnt learntdec llearnt t d =
  try
    Debug.dprintf0 debug "[Scheduler] Apply previously learnt";
    let iter_learnt n =
      Debug.dprintf2 debug "[Scheduler] @[Apply %a@]"
        Conflict.Learnt.pp n;
      Conflict.apply_learnt d n;
      S.Backtrackable.flush d in
    Bag.iter iter_learnt llearnt;
    if Conflict.learnt_is_already_true d learntdec
    then assert false; (** absurd: If it is already true it should not be this conflict *)
    iter_learnt learntdec;
    run_until_dec t d;
    Debug.dprintf0 debug_pushpop "[Scheduler] Learnt applied";
    (** TODO: decision on the last decision if it is multiple theory *)
    d
  with S.Contradiction pexp ->
    Debug.dprintf0 debug "[Scheduler] Contradiction during apply learnt";
    conflict_analysis t pexp

and pop_to t prev =
  Debug.dprintf2 debug_pushpop "[Scheduler] pop from %a"
    print_level t;
  t.wakeup_daemons <- prev.pre_wakeup_daemons;
  t.prev_scheduler_state <- prev.pre_prev_scheduler_state;
  Context.pop prev.pre_backtrack_point;
  t.learnt <- prev.pre_learnt;
  let d = new_delayed t in
  Egraph.Backtrackable.draw_graph t.solver_state;
  Debug.dprintf2 debug_pushpop "[Scheduler] pop to %a"
    print_level t;
  d

(*
  and conflict_analysis t pexp =
    Debug.incr stats_con;
    let learnt,_tags, _decs = Conflict.analyse t.solver_state pexp in
    let learnt,maxage = match learnt with
      | None -> raise Contradiction
      | Some learntmaxage -> learntmaxage in
    let rec rewind llearnt maxage prevo =
      match prevo with
      | None -> raise Contradiction
      | Some prev when
          Trail.current_age
            (S.get_trail prev.pre_solver_state) <= maxage ->
        llearnt,prev
      | Some prev ->
        let llearnt = List.rev_append llearnt prev.pre_learnt in
        rewind llearnt maxage prev.pre_prev_scheduler_state
    in
    let llearnt,prev =
      rewind (learnt::t.learnt) maxage t.prev_scheduler_state in
    Debug.dprintf2 debug_pushpop "[Scheduler] Pop to level %a"
      Trail.Age.pp maxage;
    pop_to llearnt t prev
*)

and conflict_analysis t pexp =
  if Debug.test_flag debug_dotgui then
    S.Backtrackable.draw_graph ~force:true t.solver_state;
  Debug.incr stats_con;
  if Egraph.Backtrackable.current_nbdec t.solver_state = 0 then begin
    Debug.dprintf0 debug "[Scheduler] contradiction at level 0";
    raise Contradiction
  end
  else
    let backlevel, learnt, useful =
      Conflict.learn
        (Egraph.Backtrackable.get_getter t.solver_state)
        (Egraph.Backtrackable.get_trail t.solver_state)
        pexp
    in
    t.var_inc := !(t.var_inc) *. var_decay;
    Bag.iter (update_prio t) useful;
    (** We look for the level just below the backtrack level *)
    let rec rewind t learnt llearnt prevo =
      match prevo with
      | None ->
        Debug.dprintf0 debug "[Scheduler] learnt clause false at level 0";
        raise Contradiction
      | Some prev ->
        let llearnt_all = Bag.concat llearnt prev.pre_learnt in
        let age_dec = prev.pre_age_dec in
        if Trail.Age.(backlevel < age_dec) then
          rewind t learnt llearnt_all prev.pre_prev_scheduler_state
        else
          let d = pop_to t prev in
          t.learnt <- Bag.append llearnt_all learnt;
          d,learnt,llearnt
    in
    let d,learntdec,llearnt =
      rewind t learnt t.learnt t.prev_scheduler_state in
    t.wakeup_daemons <- Prio.reprio t.decprio t.wakeup_daemons;
    apply_learnt learntdec llearnt t d

and try_run_dec:
  t -> S.t -> Prio.t -> Trail.chogen -> S.t = fun t d prio chogen ->
    (** First we verify its the decision is at this point needed *)
    try
      match Conflict.choose_decision d chogen with
      | Conflict.DecNo ->
        t.wakeup_daemons <- prio;
        d (** d can be precised by choose_decision *)
      | Conflict.DecTodo todo ->
        Debug.incr stats_dec;
        S.Backtrackable.delayed_stop d;
        (** The registered state keep the old prio *)
        push t chogen;
        (** We use the priority list without the decision only in the
            branch where the decision is made *)
        t.wakeup_daemons <- prio;
        let declevel = S.Backtrackable.new_dec t.solver_state in
        Debug.dprintf4 debug_pushpop
          "[Scheduler] Make decision: decision %a level %a"
          Trail.print_dec declevel
          print_level t;
        assert (Egraph.Backtrackable.current_nbdec t.solver_state > 0);
        let d = new_delayed t in
        todo d;
        d
    with S.Contradiction pexp ->
      Debug.dprintf0 debug "[Scheduler] Contradiction";
      conflict_analysis t pexp

and run_until_dec t d =
  let act = Prio.min t.wakeup_daemons in
  match act with
  | Some (Att.Daemon (_,att)) -> begin
      let _, prio = Prio.extract_min t.wakeup_daemons in
      Debug.incr stats_propa;
      t.wakeup_daemons <- prio;
      S.Backtrackable.run_daemon d att;
      S.Backtrackable.flush d;
      run_until_dec t d
    end
  | Some (Att.Decision (_,_)) | None -> ()


let run_one_step t d =
  let act, prio = Prio.extract_min t.wakeup_daemons in
  match act with
  | Att.Daemon (_,att) -> begin
      Debug.incr stats_propa;
      t.wakeup_daemons <- prio;
      try
        S.Backtrackable.run_daemon d att; d
      with S.Contradiction pexp ->
        Debug.dprintf0 debug "[Scheduler] Contradiction";
        conflict_analysis t pexp
    end
  | Att.Decision (_,chogen) -> try_run_dec t d prio chogen

let rec flush t d =
  try
    S.Backtrackable.flush d; d
  with S.Contradiction pexp ->
    Debug.dprintf0 debug "[Scheduler] Contradiction";
    let d = conflict_analysis t pexp in
    flush t d

exception ReachStepLimit

let rec run_inf_step ?limit ~nodec t d =
  (match limit with | Some n when n <= 0 -> raise ReachStepLimit | _ -> ());
  let d = flush t d in
  let run =
    match Prio.min t.wakeup_daemons with
    | Some (Att.Decision _) -> not nodec
    | Some (Att.Daemon _) -> true
    | None -> false
  in
  if run
  then
    let d = run_one_step t d in
    run_inf_step ?limit:(Opt.map pred limit) ~nodec t d
  else begin
    S.Backtrackable.delayed_stop d
  end

let run_inf_step ?limit ?(nodec=false) t =
  if t.delayed <> None then raise NeedStopDelayed;
  let d = new_delayed t in
  try
    run_inf_step ?limit  ~nodec t d;
    Debug.dprintf0 debug_pushpop "[Scheduler] sat";
  with (Contradiction as e) ->
    Debug.dprintf0 debug_pushpop "[Scheduler] unsat";
    raise e

let get_delayed t =
  match t.delayed with
  | Some d -> d
  | None   ->
    let d = new_delayed t in
    t.delayed <- Some d;
    d

let flush_delayed t =
  match t.delayed with
  | None -> ()
  | Some d ->
    t.delayed <- Some (flush t d)

let stop_delayed t =
  match t.delayed with
  | None -> ()
  | Some d ->
    let d = flush t d in
    S.Backtrackable.delayed_stop d;
    t.delayed <- None

let run_exn ?nodec ?limit ~theories f =
  let t = new_solver () in
  begin try
      let d = get_delayed t in
      List.iter (fun f -> f d) (SynTerm.init::theories);
      Egraph.Backtrackable.flush d;
      f d
    with S.Contradiction _ ->
      Debug.dprintf0 debug
        "[Scheduler] Contradiction during initial assertion";
      raise Contradiction
  end;
  stop_delayed t;
  run_inf_step ~nodec:(nodec=Some ()) ?limit t;
  get_delayed t

let run ?nodec ?limit ~theories f =
  try
    `Done (run_exn ?nodec ?limit ~theories f)
  with Contradiction ->
    `Contradiction
