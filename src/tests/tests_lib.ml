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

open OUnit
open Witan_popop_lib
open Witan_core

let debug = Debug.register_flag
  ~desc:" Run the test in verbose mode." "ounit"

let (&:) s l = s >::: (List.map (fun f -> TestCase f) l)

let ty_ctr = Ty.Constr.create "a"
let ty = Ty.ctr ty_ctr

let register d cl =
  Egraph.Delayed.register d cl;
  Egraph.Delayed.flush d

let merge d cl1 cl2 =
  Egraph.Delayed.merge d Trail.pexpfact cl1 cl2;
  Egraph.Delayed.flush d

let is_equal = Egraph.Delayed.is_equal

(** without decisions *)
type t =
  { wakeup_daemons    : Events.Wait.daemon_key Queue.t;
    solver_state      : Egraph.t;
  }


let new_solver () = {
  wakeup_daemons = Queue.create ();
  solver_state = Egraph.new_t ();
}

let new_delayed t =
  let sched_daemon dem = Queue.push dem t.wakeup_daemons in
  let sched_decision _ = () in
  Egraph.new_delayed ~sched_daemon ~sched_decision t.solver_state

exception ReachStepLimit
exception Contradiction

let rec run_inf_step ?limit t d =
  (match limit with | Some n when n <= 0 -> raise ReachStepLimit | _ -> ());
  Egraph.flush d;
  match Queue.pop t.wakeup_daemons with
  | exception Queue.Empty -> ()
  | dem ->
    Egraph.run_daemon d dem;
    run_inf_step ?limit:(Opt.map pred limit) t d

let run_exn ~theories f =
  let t = new_solver () in
  begin try
      let d = new_delayed t in
      List.iter (fun f -> f d) theories;
      Egraph.flush d;
      f d;
      Egraph.flush d;
      Egraph.delayed_stop d
    with Egraph.Contradiction _ ->
      Debug.dprintf0 debug
        "[Scheduler] Contradiction during initial assertion";
      raise Contradiction
  end;
  let d = new_delayed t in
  run_inf_step t d;
  d
