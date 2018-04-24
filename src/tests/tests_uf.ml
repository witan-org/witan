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
open Witan_core
open Tests_lib
open Witan_theories_bool

let theories = [Bool.th_register; Equality.th_register; Uninterp.th_register ]
let run = Tests_lib.run_exn ~theories ~nodec:()
let ($$) f x = f x

let a = SynTerm.node_of_term (Term.const (Id.mk "a" Bool.ty))
let b = SynTerm.node_of_term (Term.const (Id.mk "b" Bool.ty))
let c = SynTerm.node_of_term (Term.const (Id.mk "c" Bool.ty))

let empty () =
  let env = run $$ fun env ->
      register env a; register env b;
  in
  assert_bool "⊬ a == b" (not (is_equal env a b))

let tauto () =
  let env = run $$ fun env ->
      register env a; register env b;
      merge env a b;
  in
  assert_bool "a = b => a = b"
  (is_equal env a b)

let tauto_equal () =
  let env = run $$ fun env ->
      register env a; register env b;
      merge env a b;
  in
  assert_bool "a = b => a = b"
  (is_equal env a b)

let trans () =
  let env = run $$ fun env ->
      register env a; register env b; register env c;
      merge env a b; merge env b c;
  in
  assert_bool "a = b => b = c => a = c" (is_equal env a c);
  assert_bool "a = b => b = c => a = b" (is_equal env a b);
  assert_bool "a = b => b = c => b = c" (is_equal env b c)

let noteq () =
  let env = run $$ fun env ->
      register env a; register env b; register env c;
      merge env b c;
  in
  assert_bool "b = c => a != c" (not (is_equal env a c))

let basic = "Uf.Basic" >::: ["empty" >:: empty; "tauto" >:: tauto;
                         "trans" >:: trans; "noteq" >:: noteq]
(*
let f  = Uninterp.fun1 ty "f"
let fa = f a
let fb = f b
let ffa = f (f a)
let ffb = f (f b)

let refl () =
  let env = run $$ fun env ->
      register env fa;
  in
  assert_bool "f(a) = f(a)" (is_equal env fa fa)

let empty () =
  let env = run $$ fun env ->
      register env fa; register env fb;
  in
  assert_bool "f(a) != f(b)" (not (is_equal env fa fb))

let congru () =
  let env = run $$ fun env ->
      Shuffle.seql [
        (fun () -> register env fa);
        (fun () -> register env fb);
        (fun () -> register env a);
        (fun () -> register env b);
      ];
      merge env a b;
  in
  assert_bool "a = b => f(a) = f(b)"
    (is_equal env fa fb)

let _2level () =
  let env = run $$ fun env ->
      Shuffle.seql [
        (fun () -> register env ffa);
        (fun () -> register env ffb);
        (fun () -> register env a);
        (fun () -> register env b);
      ];
      merge env a b;
  in
  assert_bool "a = b => f(f(a)) = f(f(b))"
  (is_equal env ffa ffb)

let _2level' () =
  let env = run $$ fun env ->
      register env a; register env b;
      merge env a b;
      register env ffa; register env ffb;
  in
  assert_bool "a = b => f(f(a)) = f(f(b))" (is_equal env ffa ffb)


let bigger () =
  let rec bf n = if n < 1 then a else (f (bf(n-1))) in
  let fa = bf 1 in
  let fffa = bf 3 in
  let fffffa = bf 5 in
  let env = run $$ fun env ->
      Shuffle.seql
        [(fun () -> register env a);
         (fun () -> register env fa);
         (fun () -> register env fffa);
         (fun () -> register env fffffa);
        ];
      Shuffle.seql
        [(fun () -> merge env a fffa);
         (fun () -> merge env fffffa a)];
  in
  assert_bool "a = f(f(f(a))) => f(f(f(f(f(a))))) = a => f(a) = a"
    (is_equal env fa a)




let congru1 = "Uf.Congru 1 arg" >::: ["refl" >:: refl; "congru" >:: congru;
                                   "2level" >:: _2level; "2level'" >:: _2level';
                                   "bigger" >:: bigger]


let g  = Uninterp.fun2 ty "g"
let gab = g a b
let gaa = g a a
let ggabb = g gab b

let refl () =
  let env = run $$ fun env -> register env gab in
  assert_bool "g(a,b) = g(a,b)" (is_equal env gab gab)

let congru () =
  let env = run $$ fun env ->
      Shuffle.seql [
        (fun () -> register env gab);
        (fun () -> register env gaa);
        (fun () -> register env a);
        (fun () -> register env b);
      ];
      merge env a b;
  in
  assert_bool "a = b => g(a,b) = g(a,a)"
    (is_equal env gab gaa)

let notcongru () =
  let env = run $$ fun env ->
      register env a; register env gab; register env gaa;
      merge env a gab;
  in
  assert_bool "a = g(a,b) => g(a,b) != g(a,a)"
    (not (is_equal env gab gaa))

let _2level () =
  let env = run $$ fun env ->
      Shuffle.seql [
        (fun () -> register env a);
        (fun () -> register env gab);
      ];
      Shuffle.seql [
        (fun () -> merge env gab a;);
        (fun () -> register env ggabb);
      ];
  in
  assert_bool "g(a,b) = a => g(g(a,b),b) = a"
    (is_equal env ggabb a)

let congru2 = "Uf.congru 2 args" >::: ["refl" >:: refl; "congru" >:: congru;
                                    "2level" >:: _2level;]

let merge env x y =
  register env x;
  register env y;
  merge env x y

let x = Variable.fresh ty "x"
let y = Variable.fresh ty "y"

let altergo0 () =
  let h = Uninterp.fun1 ty "h" in
  let g = Uninterp.fun2 ty "g" in
  let gax = g a x in
  let hx = h x in
  let gahx = g a hx in
  let ggahxx = g gahx x in
  let env = run $$ fun env ->
      Shuffle.seql
        [(fun () -> merge env hx x);
         (fun () -> merge env gax a)];
      register env ggahxx
  in
  assert_bool "h(x)=x and g(a,x)=a ->  g(g(a,h(x)),x)=a"
    (is_equal env ggahxx a)

let altergo1 () =
  let h = Uninterp.fun2 ty "h" in
  let rec bf n = if n < 1 then a else f (bf (n-1)) in
  let fffa = bf 3 in
  let fffffa = bf 5 in
  let gxy = g x y in
  let ggxyy = g gxy y in
  let hggxyya = h ggxyy a in
  let fa = f a in
  let hxfa = h x fa in
  let env = run $$ fun env ->
      Shuffle.seql
        [(fun () -> merge env fffa a);
         (fun () -> merge env fffffa a);
         (fun () -> merge env gxy x)];
      register env hggxyya;
      register env hxfa;
  in
  assert_bool
    "f(f(f(a)))=a and f(f(f(f(f(a)))))=a and g(x,y)=x -> \
     h(g(g(x,y),y),a)=h(x,f(a))"
    (is_equal env hggxyya hxfa)

let altergo2 () =
  let h = Uninterp.fun2 ty "h" in
  let gxy = g x y in
  let fa = f a in
  let ggxyy = g gxy y in
  let hggxyya = h ggxyy a in
  let hxfa = h x fa in
  let env = run $$ fun env ->
      Shuffle.seql
        [(fun () -> merge env fa a);
         (fun () -> merge env gxy x)];
      register env hggxyya;
      register env hxfa;
  in
  assert_bool
    "f(a) = a -> g(x,y)=x -> h(g(g(x,y),y),a)=h(x,f(a))"
    (is_equal env hggxyya hxfa)



let altergo = "Uf.altergo tests" &: [altergo0; altergo1; altergo2]

let files = []

let altergo2 = TestList (List.map Tests_lib.test_split files)
*)


let check_file filename =
  let statements = Witan_solver.Input.read
      ~language:Witan_solver.Input.Smtlib
      ~dir:(Filename.dirname filename)
      (Filename.basename filename)
  in
  try
    Witan_solver.Notypecheck.run ~theories ~limit:1000 statements
  with
  | Witan_solver.Notypecheck.Typing_error (msg, _, t) ->
    assert_failure
      (Format.asprintf
         "%a:@\n%s:@ %a"
         Dolmen.ParseLocation.fmt (Witan_solver.Notypecheck.get_loc t) msg
         Dolmen.Term.print t
      )

let tests_smt2 expected dir =
  let files = Sys.readdir dir in
  Array.sort String.compare files;
  let files = Array.to_list files in
  List.map
    (fun s ->
      s >: TestCase (fun () ->
        let res = check_file (Filename.concat dir s) in
        begin match res with
        | `Sat ->   Witan_popop_lib.Debug.dprintf1 Tests_lib.debug "@[%s: Sat@]" s
        | `Unsat -> Witan_popop_lib.Debug.dprintf1 Tests_lib.debug "@[%s: Unsat@]" s
        end;
        assert_bool s (res = expected);
      )) files


let smtlib2sat =
  "smtlib2-uf-sat" >:::
    tests_smt2 `Sat "solve/smt_uf/sat/"

let smtlib2unsat =
  "smtlib2-uf-unsat" >:::
    tests_smt2 `Unsat "solve/smt_uf/unsat/"


let tests = TestList [basic;(* congru1;congru2;altergo;altergo2;*)
                             smtlib2sat; smtlib2unsat]
