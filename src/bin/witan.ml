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

let theories = [(* Uninterp.th_register; *) Witan_theories_bool.Bool.th_register]

let () =
  (* Parse command line options *)
  let opts = match Cmdliner.Term.eval (Options.all, Options.info) with
    | `Version | `Help -> exit 0
    | `Error `Parse
    | `Error `Term
    | `Error `Exn -> exit 1
    | `Ok opts -> opts
  in
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
      ~limit:1000
      (fun d ->
         Gen.iter (fun stmt ->
             let open Dolmen.Statement in
             match stmt.descr with
             | Clause l ->
               let map t =
                 match Witan_solver.Notypecheck.parse_formula env t with
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
               Egraph.Delayed.register d cl;
               Witan_theories_bool.Bool.set_true d Trail.pexp_fact cl
             | _ -> ())
           statements) in
  match res with
  | `Contradiction -> Printf.printf "unsat\n"
  | `Done d ->
    Format.printf "sat@.";
    let model = Witan_solver.Notypecheck.get_model env d in
    Format.printf "(%a)@."
      Witan_popop_lib.Pp.(iter22 Witan_core.Term.H.iter space
                            (fun fmt t v -> Format.fprintf fmt "(%a %a)"
                                Witan_core.Term.pp t Witan_core.Values.pp v))
      model
