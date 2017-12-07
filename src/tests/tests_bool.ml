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
open Witan_stdlib
open Witan_core
open Tests_lib
open Witan_theories_bool

let theories = [Variable.th_register; (* Uninterp.th_register; *) Bool.th_register]

let ($$) f x = f x

let run = Tests_lib.run_exn ~theories

let true_is_true () =
  let env = run (fun _ -> ()) in
  assert_bool "" (Bool.is_true env Bool._true);
  assert_bool "" (not (Bool.is_false env Bool._true))

let not_true_is_false () =
  let not_true = Bool._not Bool._true in
  let env = run $$ fun env -> Egraph.Delayed.register env not_true in
  assert_bool "" (Bool.is_false env not_true);
  assert_bool "" (not (Bool.is_true env not_true))

let and_true_is_true () =
  let _t = Bool._true in
  let _and = Bool._and [_t;_t;_t] in
  let env = run $$ fun env -> Egraph.Delayed.register env _and in
  assert_bool "" (Bool.is_true env _and);
  assert_bool "" (not (Bool.is_false env _and))

let or_not_true_is_false () =
  let _f = (Bool._not Bool._true) in
  let _or = Bool._and [_f;_f;_f] in
  let env = run $$ fun env -> Egraph.Delayed.register env _or in
  assert_bool "" (Bool.is_false env _or);
  assert_bool "" (not (Bool.is_true env _or))

let merge_true () =
  let a  = Variable.fresh Bool.ty "a" in
  let b  = Variable.fresh Bool.ty "b" in
  let c  = Variable.fresh Bool.ty "c" in
  let d  = Variable.fresh Bool.ty "d" in
  let _and = Bool._and [a;b;c] in
  let env = run $$ fun env ->
      Egraph.Delayed.register env _and;
      List.iter (Egraph.Delayed.register env) [a;b;c;d];
      Shuffle.seql
        [(fun () -> merge env a b);
         (fun () -> merge env a c);
        ];
      merge env a d;
      Bool.set_true env Trail.pexpfact d;
  in
  assert_bool "" (Bool.is_true env _and)

let imply_implies () =
  let a = Term.const (Id.mk "a" Term._Prop) in
  let b = Term.const (Id.mk "b" Term._Prop) in
  let t = Term.apply Term.imply_term [a;b] in
  let an = Synsem.of_term a in
  let bn = Synsem.of_term b in
  let tn = Synsem.of_term t in
  let env = run $$ fun env ->
      Egraph.Delayed.register env tn;
      Bool.set_true env Trail.pexpfact tn;
      Egraph.Delayed.register env an;
      Bool.set_true env Trail.pexpfact an;
  in
  assert_bool "" (Bool.is_true env bn)

let basic = "Bool.Basic" >::: [ "true_is_true" >:: true_is_true;
                                "not_true_is_false" >:: not_true_is_false;
                                "and_true_is_true" >:: and_true_is_true;
                                "or_not_true_is_false" >:: or_not_true_is_false;
                                "merge_true" >:: merge_true;
                                "imply_implies" >:: imply_implies;
                                (* "modus_ponens"         >:: modus_ponens; *)
                              ]

(* let tests_dimacs expected dir = *)
(*   let files = Sys.readdir dir in *)
(*   Array.sort String.compare files; *)
(*   let files = Array.to_list files in *)
(*   List.map *)
(*     (fun s -> *)
(*       s >: TestCase (fun () -> *)
(*         let res = Dimacs.check_file (Filename.concat dir s) in *)
(*         begin match res with *)
(*         | Dimacs.Sat ->   Debug.dprintf1 Tests_lib.debug "@[%s: Sat@]" s *)
(*         | Dimacs.Unsat -> Debug.dprintf1 Tests_lib.debug "@[%s: Unsat@]" s *)
(*         end; *)
(*         assert_bool s (res = expected); *)
(*       )) files *)

(* let dimacssat = *)
(*   "dimacs-sat" >::: tests_dimacs Dimacs.Sat "tests/dimacs/sat/" *)

(* let dimacsunsat = *)
(*   "dimacs-unsat" >::: tests_dimacs Dimacs.Unsat "tests/dimacs/unsat/" *)

let tests = TestList [basic(* ;dimacssat;dimacsunsat *)]
