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
    then Some (Values.index Bool.dom a Bool.ty)
    else if Term.equal t tb
    then Some (Values.index Bool.dom b Bool.ty)
    else if Term.equal t tc
    then Some (Values.index Bool.dom c Bool.ty)
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
    match Values.value Bool.dom v' with
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
  let a  = fresh Bool.ty "a" in
  let b  = fresh Bool.ty "b" in
  let c  = fresh Bool.ty "c" in
  let d  = fresh Bool.ty "d" in
  let _and = Bool._and [a;b;c] in
  let env = run $$ fun env ->
      Egraph.Delayed.register env _and;
      List.iter (Egraph.Delayed.register env) [a;b;c;d];
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
      Egraph.Delayed.register env tn;
      Bool.set_true env Trail.pexp_fact tn;
      Egraph.Delayed.register env an;
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
(* Exception for typing errors *)
module R = Witan_popop_lib.Exthtbl.Hashtbl.Make(Dolmen.Id)
type env = Term.Id.t R.t

let create_env () =
  R.create 10

exception Typing_error of string * env * Dolmen.Term.t

let _bad_op_arity env s n t =
  let msg = Format.asprintf "Bad arity for operator '%s' (expected %d arguments)" s n in
  raise (Typing_error (msg, env, t))

(** no typing *)
let rec parse_formula (env:env) (t:Dolmen.Term.t) =
  let module Ast = Dolmen.Term in
  let open Term in
  match t with

  (* Ttype & builtin types *)
  | { Ast.term = Ast.Builtin Ast.Ttype } ->
    _Type
  | { Ast.term = Ast.Builtin Ast.Prop } ->
    _Prop

  (* Basic formulas *)
  | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.True }, []) }
  | { Ast.term = Ast.Builtin Ast.True } ->
    true_term

  | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.False }, []) }
  | { Ast.term = Ast.Builtin Ast.False } ->
    false_term

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.And}, l) } ->
    apply and_term (List.map (parse_formula env) l)

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Or}, l) } ->
    apply or_term (List.map (parse_formula env) l)

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Xor}, l) } as t ->
    begin match l with
      | [p; q] ->
        let f = parse_formula env p in
        let g = parse_formula env q in
        apply not_term [apply equal_term [f;g]]
      | _ -> _bad_op_arity env "xor" 2 t
    end

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Imply}, l) } as t ->
    begin match l with
      | [p; q] ->
        let f = parse_formula env p in
        let g = parse_formula env q in
        apply imply_term [f;g]
      | _ -> _bad_op_arity env "=>" 2 t
    end

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Equiv}, l) } as t ->
    begin match l with
      | [p; q] ->
        let f = parse_formula env p in
        let g = parse_formula env q in
        apply equiv_term [f;g]
      | _ -> _bad_op_arity env "<=>" 2 t
    end

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Not}, l) } as t ->
    begin match l with
      | [p] ->
        apply not_term [parse_formula env p]
      | _ -> _bad_op_arity env "not" 1 t
    end

  (* (\* Binders *\)
   * | { Ast.term = Ast.Binder (Ast.All, vars, f) } ->
   *   let ttype_vars, ty_vars, env' =
   *     parse_quant_vars (expect env (Typed Expr.Ty.base)) vars in
   *   Formula (
   *     mk_quant_ty env' Expr.Formula.allty ttype_vars
   *       (mk_quant_term env' Expr.Formula.all ty_vars
   *          (parse_formula env' f)))
   * 
   * | { Ast.term = Ast.Binder (Ast.Ex, vars, f) } ->
   *   let ttype_vars, ty_vars, env' =
   *     parse_quant_vars (expect env (Typed Expr.Ty.base)) vars in
   *   Formula (
   *     mk_quant_ty env' Expr.Formula.exty ttype_vars
   *       (mk_quant_term env' Expr.Formula.ex ty_vars
   *          (parse_formula env' f))) *)

  (* (Dis)Equality *)
  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Eq}, l) } as t ->
    begin match l with
      | [_; _] ->
        apply equal_term (List.map (parse_formula env) l)
      (* begin match promote env t @@ parse_expr env a,
       *             promote env t @@ parse_expr env b with
       *   | Term t1, Term t2 ->
       *     Formula (make_eq env t t1 t2)
       *   | Formula f1, Formula f2 ->
       *     Formula (Expr.Formula.equiv f1 f2)
       *   | _ ->
       *     _expected env "either two terms or two formulas" t None
       * end *)
      | _ -> _bad_op_arity env "=" 2 t
    end

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Distinct}, args) } ->
    apply distinct_term (List.map (parse_formula env) args)

  (* General case: application *)
  | { Ast.term = Ast.Symbol s } ->
    let id = R.memo (fun id ->
        let s = Format.asprintf "%a" Dolmen.Id.print id in
        Witan_core.Id.mk s _Prop) env s in
    const id
  (* | { Ast.term = Ast.App ({ Ast.term = Ast.Symbol s }, l) } as ast ->
   *   parse_app env ast s l *)

  (* (\* Local bindings *\)
   * | { Ast.term = Ast.Binder (Ast.Let, vars, f) } ->
   *   parse_let env f vars *)

  (* Other cases *)
  | ast -> raise (Typing_error ("Unexpected construction", env, ast))

let get_loc =
  let default_loc = Dolmen.ParseLocation.mk "<?>" 0 0 0 0 in
  (fun t -> CCOpt.get_or ~default:default_loc t.Dolmen.Term.loc)

(** used to complete partial model *)
let default_value = true

let get_model env d =
  let model : Values.t Term.H.t = Term.H.create 16 in
  R.iter (fun _ id ->
      let t = Term.const id in
      let n = SynTerm.node_of_term t in
      let v = Egraph.Delayed.get_value d Bool.dom n in
      let v = Witan_popop_lib.Opt.get_def default_value v in
      let v = if v then Bool.values_true else Bool.values_false in
      Term.H.add_new Std.Impossible model t v)
    env;
  model

let check_model model expected n =
  let leaf t = Term.H.find_opt model t in
  assert_bool "check_model" (Values.equal (Interp.node ~leaf n) expected)

let check_file filename =
  let statements = Witan_solver.Input.read
      ~language:Witan_solver.Input.Dimacs
      ~dir:(Filename.dirname filename)
      (Filename.basename filename)
  in
  let env = create_env () in
  let clauses = ref [] in
  let res =
    Witan_solver.Scheduler.run ~theories
      (fun d ->
         Gen.iter (fun stmt ->
             let open Dolmen.Statement in
             match stmt.descr with
             | Clause l ->
               let map t =
                 match parse_formula env t with
                 | exception (Typing_error (msg, _, t)) ->
                   assert_failure
                     (Format.asprintf
                        "%a:@\n%s:@ %a"
                        Dolmen.ParseLocation.fmt (get_loc t) msg
                        Dolmen.Term.print t
                     );
                 | t ->
                   SynTerm.node_of_term t
               in
               let l = Shuffle.shufflel l in
               let l = List.map map l in
               let l = Shuffle.shufflel l in
               let cl = Bool._or l in
               clauses := cl::!clauses;
               Egraph.Delayed.register d cl;
               Bool.set_true d Trail.pexp_fact cl
             | _ -> ())
           statements) in
  match res with
  | `Contradiction -> `Unsat
  | `Done d ->
    let model = get_model env d in
    List.iter (check_model model Bool.values_true) !clauses;
    `Sat

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
