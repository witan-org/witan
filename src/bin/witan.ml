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

let theories = Witan_theories_bool.[Bool.th_register; Equality.th_register; Uninterp.th_register ]


let () =
  if not Witan_core.(Egraph.check_initialization () &&
                     Conflict.check_initialization ()) then
    exit 1


let () =
  (* Parse command line options *)
  let opts = match Cmdliner.Term.eval (Options.all, Options.info) with
    | `Version | `Help -> exit 0
    | `Error `Parse
    | `Error `Term
    | `Error `Exn -> exit 1
    | `Ok opts -> opts
  in
  List.iter (fun f -> Witan_popop_lib.Debug.set_flag f) opts.Options.debug_flags;
  Witan_popop_lib.Debug.(if test_flag stack_trace then Printexc.record_backtrace true);
  begin match opts.Options.seed_shuffle with
    | None   -> Witan_stdlib.Shuffle.set_shuffle None;
    | Some i ->  Witan_stdlib.Shuffle.set_shuffle (Some [|i|]);
  end;
  (* Parse input *)
  let statements = Witan_solver.Input.read
      ?language:Options.(opts.input.language)
      ~dir:Options.(opts.input.dir)
      Options.(opts.input.file)
  in
  if opts.Options.type_only then exit 0;
  let env = Witan_solver.Notypecheck.create_env () in
  let clauses = ref [] in
  let open Witan_core in
    let res =
    Witan_solver.Scheduler.run
      ~theories
      ?limit:(if opts.Options.step_limit < 0 then None else Some opts.Options.step_limit)
      (fun d ->
         Gen.iter (fun stmt ->
             let open Dolmen.Statement in
             match stmt.descr with
             | Set_logic _ -> ()
             | Set_info _ -> ()
             | Prove -> ()
             | Dolmen.Statement.Exit -> ()
             | Decl (id,t) ->
               let t = Dolmen.Normalize.smtlib t in
               let ty =
                 match Witan_solver.Notypecheck.parse_formula env Witan_solver.Notypecheck.MId.empty t with
                 | exception (Witan_solver.Notypecheck.Typing_error (msg, _, t)) ->
                   Format.eprintf
                     "%a:@\n%s:@ %a"
                     Dolmen.ParseLocation.fmt (Witan_solver.Notypecheck.get_loc t) msg
                     Dolmen.Term.print t;
                   Pervasives.exit 2
                 | t ->
                   t
               in
               let t' =
                 let s = Format.asprintf "%a" Dolmen.Id.print id in
                 Witan_core.Id.mk s ty
               in
               Witan_solver.Notypecheck.R.add_new Witan_stdlib.Std.Impossible env id t';
             | Clause l ->
               let map t =
                 match Witan_solver.Notypecheck.parse_formula env Witan_solver.Notypecheck.MId.empty t with
                 | exception (Witan_solver.Notypecheck.Typing_error (msg, _, t)) ->
                   Format.eprintf
                     "%a:@\n%s:@ %a"
                     Dolmen.ParseLocation.fmt (Witan_solver.Notypecheck.get_loc t) msg
                     Dolmen.Term.print t;
                   Pervasives.exit 2
                 | t ->
                   SynTerm.node_of_term t
               in
               let l = Witan_stdlib.Shuffle.shufflel l in
               let l = List.map map l in
               let l = Witan_stdlib.Shuffle.shufflel l in
               let cl = Witan_theories_bool.Bool._or l in
               clauses := cl::!clauses;
               Egraph.register d cl;
               Witan_theories_bool.Bool.set_true d Trail.pexp_fact cl
             | Antecedent t ->
               let map t =
                 match Witan_solver.Notypecheck.parse_formula env Witan_solver.Notypecheck.MId.empty t with
                 | exception (Witan_solver.Notypecheck.Typing_error (msg, _, t)) ->
                   Format.eprintf
                     "%a:@\n%s:@ %a"
                     Dolmen.ParseLocation.fmt (Witan_solver.Notypecheck.get_loc t) msg
                     Dolmen.Term.print t;
                   Pervasives.exit 2
                 | t ->
                   SynTerm.node_of_term t
               in
               let t = Dolmen.Normalize.smtlib t in
               let cl = map t in
               clauses := cl::!clauses;
               Egraph.register d cl;
               Witan_theories_bool.Bool.set_true d Trail.pexp_fact cl
             | _ -> invalid_arg (Format.asprintf "Unimplemented command: %a" Dolmen.Statement.print stmt))
           statements) in
  match res with
  | `Contradiction -> Printf.printf "unsat\n"
  | `Done _d ->
    Format.printf "sat@.";
    (* let model = Witan_solver.Notypecheck.get_model env d in
     * Format.printf "(%a)@."
     *   Witan_popop_lib.Pp.(iter22 Witan_core.Term.H.iter space
     *                         (fun fmt t v -> Format.fprintf fmt "(%a %a)"
     *                             Witan_core.Term.pp t Witan_core.Values.pp v))
     *   model *)
    ()
