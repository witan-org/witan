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

let theories = Witan_theories_bool.[Boolean.th_register; Equality.th_register; Uninterp.th_register ]


let () =
  if not Witan_core.(Egraph.check_initialization () &&
                     Conflict.check_initialization ()) then
    exit 1


let one_file opts file =
  (* Parse input *)
  let statements = Witan_solver.Input.read
      ?language:Options.(opts.input.language)
      ~dir:Options.(opts.input.dir)
      file
  in
  if opts.Options.type_only then exit 0;
  let res =
    Witan_solver.Notypecheck.run
      ?limit:(if opts.Options.step_limit < 0 then None else Some opts.Options.step_limit)
      ~theories statements in
  match res with
  | `Unsat -> Printf.printf "unsat\n"
  | `Sat -> Printf.printf "sat\n"

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
  one_file opts Options.(opts.input.file)
