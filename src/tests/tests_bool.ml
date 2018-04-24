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

let theories = [(* Uninterp.th_register; *) Bool.th_register]

let ($$) f x = f x

let run = Tests_lib.run_exn ~theories

let bool_interp () =
  let ta = Term.const (Id.mk "a" Bool.ty) in
  let tb = Term.const (Id.mk "b" Bool.ty) in
  let tc = Term.const (Id.mk "c" Bool.ty) in
  let to_n x = SynTerm.node_of_term x in
  let na = to_n ta in
  let nb = to_n tb in
  let nc = to_n tc in
  let leaf ~a ~b ~c t =
    if Term.equal t ta
    then Some (Value.index Bool.dom a Bool.ty)
    else if Term.equal t tb
    then Some (Value.index Bool.dom b Bool.ty)
    else if Term.equal t tc
    then Some (Value.index Bool.dom c Bool.ty)
    else None
  in
  let l = [
    "true", Bool._true, true, (fun _-> None);
    "false", Bool._false, false, (fun _-> None);
    "or(a,b,c)", Bool._or [na;nb;nc], false, leaf ~a:false ~b:false ~c:false;
    "or(a,b,c)", Bool._or [na;nb;nc], true, leaf ~a:false ~b:true ~c:false;
    "not(or(a,not b,and(c,c)))",
       Bool.gen true [na,false;nb,true;(Bool._and [nc;nc]),false], true, leaf ~a:false ~b:true ~c:false;
  ]
  in
  let test (msg,n,v,leaf) =
    let v' = Interp.node ~leaf n in
    match Value.value Bool.dom v' with
    | None -> assert_failure (Printf.sprintf "Not a value of type bool: %s" msg)
    | Some v' -> assert_bool msg (v = v')
  in
  List.iter test l

let true_is_true () =
  let env = run (fun _ -> ()) in
  assert_bool "" (Bool.is_true env Bool._true);
  assert_bool "" (not (Bool.is_false env Bool._true))

let not_true_is_false () =
  let not_true = Bool._not Bool._true in
  let env = run $$ fun env -> Egraph.register env not_true in
  assert_bool "" (Bool.is_false env not_true);
  assert_bool "" (not (Bool.is_true env not_true))

let and_true_is_true () =
  let _t = Bool._true in
  let _and = Bool._and [_t;_t;_t] in
  let env = run $$ fun env -> Egraph.register env _and in
  assert_bool "" (Bool.is_true env _and);
  assert_bool "" (not (Bool.is_false env _and))

let or_not_true_is_false () =
  let _f = (Bool._not Bool._true) in
  let _or = Bool._and [_f;_f;_f] in
  let env = run $$ fun env -> Egraph.register env _or in
  assert_bool "" (Bool.is_false env _or);
  assert_bool "" (not (Bool.is_true env _or))

let merge_true () =
  let a  = fresh Bool.ty "a" in
  let b  = fresh Bool.ty "b" in
  let c  = fresh Bool.ty "c" in
  let d  = fresh Bool.ty "d" in
  let _and = Bool._and [a;b;c] in
  let env = run $$ fun env ->
      Egraph.register env _and;
      List.iter (Egraph.register env) [a;b;c;d];
      Shuffle.seql
        [(fun () -> merge env a b);
         (fun () -> merge env a c);
        ];
      merge env a d;
      Bool.set_true env Trail.pexp_fact d;
  in
  assert_bool "" (Bool.is_true env _and)

let imply_implies () =
  let a = Term.const (Id.mk "a" Term._Prop) in
  let b = Term.const (Id.mk "b" Term._Prop) in
  let t = Term.apply Term.imply_term [a;b] in
  let an = SynTerm.node_of_term a in
  let bn = SynTerm.node_of_term b in
  let tn = SynTerm.node_of_term t in
  let env = run $$ fun env ->
      Egraph.register env tn;
      Bool.set_true env Trail.pexp_fact tn;
      Egraph.register env an;
      Bool.set_true env Trail.pexp_fact an;
  in
  assert_bool "" (Bool.is_true env bn)

let basic = "Bool.Basic" >::: [ "bool_interp" >:: bool_interp;
                                "true_is_true" >:: true_is_true;
                                "not_true_is_false" >:: not_true_is_false;
                                "and_true_is_true" >:: and_true_is_true;
                                "or_not_true_is_false" >:: or_not_true_is_false;
                                "merge_true" >:: merge_true;
                                "imply_implies" >:: imply_implies;
                                (* "modus_ponens"         >:: modus_ponens; *)
                              ]


let check_file filename =
  let statements = Witan_solver.Input.read
      ~language:Witan_solver.Input.Dimacs
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

let tests_dimacs expected dir =
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

let dimacssat =
  "dimacs-sat" >::: tests_dimacs `Sat "solve/dimacs/sat/"

let dimacsunsat =
  "dimacs-unsat" >::: tests_dimacs `Unsat "solve/dimacs/unsat/"

let tests = TestList [basic; dimacssat;dimacsunsat]


let () = Witan_popop_lib.Exn_printer.register (fun fmt exn ->
    match exn with
    | Dolmen.ParseLocation.Syntax_error(l,"") ->
      Format.fprintf fmt "%a: syntax error."
        Dolmen.ParseLocation.fmt l
    | Dolmen.ParseLocation.Syntax_error(l,c) ->
      Format.fprintf fmt "%a: syntax error %s."
        Dolmen.ParseLocation.fmt l c
    | exn -> raise exn
  )
