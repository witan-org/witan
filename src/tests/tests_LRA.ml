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
open Tests_lib
open Witan_theories_bool
open Witan_theories_LRA
open Witan_solver
open Witan_core
open Witan_stdlib.Std
open Witan_stdlib

let theories = [Boolean.th_register; Equality.th_register; Uninterp.th_register; LRA.th_register]

let ($$) f x = f x

let run = Scheduler.run_exn ~nodec:() ~theories
let run_dec = Scheduler.run_exn ?nodec:None ~theories
let ($$) f x = f x

(* The tests with rundec check only the result on a model satisfying
   the hypothesis *)


let solve0a () =
  let a  = fresh Term._Real "ar" in
  let _1 = LRA.cst Q.one in
  let a1 = LRA.add a _1 in
  let env = run $$ fun env ->
      register env a1; register env _1;
      merge env a1 _1
  in
  assert_bool "a+1 = 1 => a = 0" (is_equal env a LRA.zero)

(* let solve0b () =
 *   let a  = fresh Term._Real "ar" in
 *   let _1 = LRA.cst Q.one in
 *   let _2 = LRA.cst Q.two in
 *   let _4 = LRA.cst (Q.of_int 4) in
 *   let a1 = LRA.add a _1 in
 *   let _2a2 = LRA.add' Q.two a Q.one _2 in
 *   let env = run $$ fun env ->
 *       List.iter (register env) [a1;_1;_2;_4;_2a2];
 *       merge env a1 _2
 *   in
 *   assert_bool "a+1 = 2 => 2*a+2 = 4" (is_equal env _2a2 _4) *)

let solve0c () =
  let a  = fresh Term._Real "ar" in
  let b  = fresh Term._Real "br" in
  let _1 = LRA.cst Q.one in
  let a1 = LRA.add a _1 in
  let b1 = LRA.add b _1 in
  let env = run_dec $$ fun env ->
      register env a1; register env b1;
      merge env a1 b1
  in
  assert_bool "a+1 = b+1 => a = b" (is_equal env a b)

let solve1 () =
  let a,b  = Shuffle.seq2 (fresh Term._Real) ("ar","br") in
  let _1 = LRA.cst Q.one in
  let a1 = LRA.add a _1 in
  let b1 = LRA.add b _1 in
  let _2 = LRA.cst (Q.of_int 2) in
  let a2 = LRA.add a _2 in
  let b2 = LRA.add b _2 in
  let env = run_dec $$ fun env ->
      Shuffle.seql' (register env) [a1; b1; a2; b2];
      merge env a1 b1
  in
  assert_bool "a+1 = b+1 => a+2 = b+2" (is_equal env a2 b2)

let solve2 () =
  let a,b  = Shuffle.seq2 (fresh Term._Real) ("ar","br") in
  let _1 = LRA.cst Q.one in
  let a1 = LRA.add a _1 in
  let b1 = LRA.add b _1 in
  let _2 = LRA.cst (Q.of_int 2) in
  let a2 = LRA.add a _2 in
  let b2 = LRA.add b _2 in
  let env = run_dec $$ fun env ->
      Shuffle.seql' (register env) [a1; b1; a2; b2];
      merge env a2 b1
  in
  assert_bool "a+2 = b+1 => a+1 = b" (is_equal env a1 b)

let solve3 () =
  let a,b  = Shuffle.seq2 (fresh Term._Real) ("ar","br") in
  let _1 = LRA.cst Q.one in
  let b1 = LRA.add b _1 in
  let _2 = LRA.cst (Q.of_int 2) in
  let a2 = LRA.add a _2 in
  let _3 = LRA.cst (Q.of_int 3) in
  let env = run $$ fun env ->
      Shuffle.seql [
        (fun () ->
           Shuffle.seql' (register env) [b1;a2];
           merge env a2 b1;
        );
        (fun () ->
           Shuffle.seql' (register env) [a;_2];
           merge env a _2;
        );
        (fun () ->
           register env _3;
        );
      ]
  in
  assert_bool "" (not (is_equal env b _2));
  assert_bool "a+2 = b+1 => a = 2 => b = 3" (is_equal env b _3)


let solve4 () =
  let a,b,c =
    Shuffle.seq3 (fresh Term._Real) ("ar","br","cr") in
  let t1 = LRA.cst (Q.of_int 2) in
  let t1 = LRA.add t1 c in
  let t1 = LRA.add a t1  in
  let t1' = (LRA.cst (Q.of_int 1)) in
  let t1' = LRA.add b t1' in
  let t2  = a in
  let t2' = LRA.cst (Q.of_int 2) in
  let t2' = LRA.add t2' b in
  let t3' = LRA.cst (Q.of_int (-3)) in
  let env = run_dec $$ fun env ->
      Shuffle.seql [
        (fun () ->
           Shuffle.seql' (register env) [t1;t1'];
           merge env t1 t1');
        (fun () ->
           Shuffle.seql' (register env) [t2;t2'];
           merge env t2 t2');
        (fun () -> register env t3');
      ]
  in
  assert_bool "a+(2+c) = b+1 => a = 2 + b => c = -3" (is_equal env c t3')


let solve5 () =
  let a  = fresh Term._Real "ar" in
  let b  = fresh Term._Real "br" in
  let c  = fresh Term._Real "cr" in
  let t1 = LRA.sub b c in
  let t1  = LRA.add a t1  in
  let t1' = (LRA.cst (Q.of_int 2)) in
  let t2  = a in
  let t2' = LRA.cst (Q.of_int 2) in
  let t3 = LRA.add b c in
  let t3' = LRA.add b b in
  let env = run_dec $$ fun env ->
      Shuffle.seql [
        (fun () ->
           Shuffle.seql' (register env) [t1;t1'];
           merge env t1 t1');
        (fun () ->
           Shuffle.seql' (register env) [t2;t2'];
           merge env t2 t2');
        (fun () ->
           Shuffle.seql' (register env) [t3;t3'];)
      ]
  in
  assert_bool "a+(b-c) = 2 => a = 2 => b + c = 2b" (is_equal env t3 t3')


let basic = "LRA.Basic" &:
            [solve0a;
             (* solve0b; *)
             solve0c;
             solve1;
             solve2;
             solve3;
             solve4;
             solve5
            ]

(* let mult0 () =
 *   let a  = fresh Term._Real "ar" in
 *   let b  = fresh Term._Real "br" in
 *   let t1  = LRA.sub a b  in
 *   let t1' = LRA.mult a b in
 *   let t2  = a in
 *   let t2' = LRA.cst (Q.of_int 1) in
 *   let t3 = LRA.mult_cst (Q.of_int 2) b in
 *   let t3' = LRA.cst (Q.of_int 1) in
 *   let env = run $$ fun env ->
 *       Shuffle.seql [
 *         (fun () ->
 *            Shuffle.seql' (register env) [t1;t1'];
 *            merge env t1 t1');
 *         (fun () ->
 *            Shuffle.seql' (register env) [t2;t2'];
 *            merge env t2 t2');
 *         (fun () ->
 *            Shuffle.seql' (register env) [t3;t3'];)
 *       ]
 *   in
 *   assert_bool "a - b = a * b -> a = 1 -> 1 = 2b" (is_equal env t3 t3')
 * 
 * (\** test that mult normalization trigger the needed solve *\)
 * let mult1 () =
 *   let a  = fresh Term._Real "ar" in
 *   let b  = fresh Term._Real "br" in
 *   let c  = fresh Term._Real "cr" in
 *   let t1  = LRA.mult a b  in
 *   let t1  = LRA.add a t1  in
 *   let t1' = LRA.add b c in
 *   let t1' = LRA.mult t1' a in
 *   let t2  = a in
 *   let t2' = LRA.cst (Q.of_int 2) in
 *   let t3 = c in
 *   let t3' = LRA.cst (Q.of_int 1) in
 *   let env = run $$ fun env ->
 *       Shuffle.seql [
 *         (fun () ->
 *            Shuffle.seql' (register env) [t1;t1'];
 *            merge env t1 t1');
 *         (fun () ->
 *            Shuffle.seql' (register env) [t2;t2'];
 *            merge env t2 t2');
 *         (fun () ->
 *            Shuffle.seql' (register env) [t3;t3'];)
 *       ]
 *   in
 *   assert_bool "a + (a * b) = (b + c) * a -> a = 2 -> c = 1"
 *     (is_equal env t3 t3')
 * 
 * let mult = "LRA.Mult" &: [mult0;mult1]
 * 
 * 
 * let files = ["tests/tests_altergo_arith.split";
 *              "tests/tests_popop.split";
 *              "tests/tests_altergo_qualif.split"
 *             ]
 * 
 * let altergo = TestList (List.map Tests_lib.test_split files)
*)


let check_file filename =
  let statements = Witan_solver.Input.read
      ~language:Witan_solver.Input.Smtlib
      ~dir:(Filename.dirname filename)
      (Filename.basename filename)
  in
  Witan_solver.Notypecheck.run ~theories ~limit:1000 statements

let tests_smt2 expected dir =
  if Sys.file_exists dir then
    let files = Sys.readdir dir in
    Array.sort String.compare files;
    let files = Array.to_list files in
    List.map
      (fun s ->
         s >: TestCase (fun () ->
             begin match check_file (Filename.concat dir s) with
               | `Sat ->
                 Witan_popop_lib.Debug.dprintf1 Tests_lib.debug "@[%s: Sat@]" s;
                 assert_bool s (`Sat = expected)
               | `Unsat ->
                 Witan_popop_lib.Debug.dprintf1 Tests_lib.debug "@[%s: Unsat@]" s;
                 assert_bool s (`Unsat = expected)
               | exception Witan_solver.Notypecheck.Typing_error (msg, t) ->
                 assert_string
                   (Format.asprintf
                      "%a:@\n%s:@ %a"
                      Dolmen.ParseLocation.fmt (Witan_solver.Notypecheck.get_loc t) msg
                      Dolmen.Term.print t
                   )
             end;
           )) files
  else
    []

let smtlib2sat =
  "smtlib2-lra-sat" >:::
  tests_smt2 `Sat "solve/smt_lra/sat/"

let smtlib2unsat =
  "smtlib2-lra-unsat" >:::
  tests_smt2 `Unsat  "solve/smt_lra/unsat/"

let tests = TestList [basic;(* (\* mult;*\)altergo;*) (* smtlib2sat; smtlib2unsat *)]
