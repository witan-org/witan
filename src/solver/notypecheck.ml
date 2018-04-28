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

(* Exception for typing errors *)
module R = Witan_popop_lib.Exthtbl.Hashtbl.Make(Dolmen.Id)
module MId = CCMap.Make(struct include Dolmen.Id let pp = print end)
type env = Term.Id.t R.t

let create_env () =
  R.create 10

exception Typing_error of string * Dolmen.Term.t

let _bad_op_arity _ s n t =
  let msg = Format.asprintf "Bad arity for operator '%s' (expected %d arguments)" s n in
  raise (Typing_error (msg, t))

(** no typing *)
let rec parse_formula' (env:env) (lets:Term.t MId.t) (t:Dolmen.Term.t) =
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
    let f = (and_term (List.length l)) in
    let l = (List.map (parse_formula env lets) l) in
    apply f l

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Or}, l) } ->
    apply (or_term (List.length l)) (List.map (parse_formula env lets) l)

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Xor}, l) } as t ->
    begin match l with
      | [p; q] ->
        let f = parse_formula env lets p in
        let g = parse_formula env lets q in
        apply not_term [apply equal_term [f.Term.ty;f;g]]
      | _ -> _bad_op_arity env "xor" 2 t
    end

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Imply}, l) } as t ->
    begin match l with
      | [p; q] ->
        let f = parse_formula env lets p in
        let g = parse_formula env lets q in
        apply imply_term [f;g]
      | _ -> _bad_op_arity env "=>" 2 t
    end

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Equiv}, l) } as t ->
    begin match l with
      | [p; q] ->
        let f = parse_formula env lets p in
        let g = parse_formula env lets q in
        apply equiv_term [f;g]
      | _ -> _bad_op_arity env "<=>" 2 t
    end

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Not}, l) } as t ->
    begin match l with
      | [p] ->
        apply not_term [parse_formula env lets p]
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
      | [a; b] ->
        let a = parse_formula env lets a in
        let b = parse_formula env lets b in
        apply equal_term [a.Term.ty;a;b]
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

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Distinct}, a::args) } ->
    let a = parse_formula env lets a in
    apply (distinct_term (List.length args + 1)) (a.Term.ty::a::(List.map (parse_formula env lets) args))

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Ite}, l) }
  | { Ast.term = Ast.App ({Ast.term = Ast.Symbol {Dolmen.Id.name = "ite"}}, l) } ->
    begin match l with
      | [cond;then_; else_] ->
        let cond  = parse_formula env lets cond in
        let then_ = parse_formula env lets then_ in
        let else_ = parse_formula env lets else_ in
        apply ite_term [then_.Term.ty;cond;then_;else_]
      | _ -> _bad_op_arity env "ite" 3 t
    end

  (* General case: application *)
  | { Ast.term = Ast.Symbol s }
  | { Ast.term = Ast.App ({ Ast.term = Ast.Symbol s }, []) } ->
    begin match MId.find_opt s lets with
    | None ->
      let id = R.memo (fun id ->
          let s = Format.asprintf "%a" Dolmen.Id.print id in
          (** only in dimacs they are not declared *)
          Witan_core.Id.mk s _Prop) env s in
      const id
    | Some t -> t
    end

  | { Ast.term = Ast.App ({ Ast.term = Ast.Symbol s }, l) } as ast ->
    begin match MId.find_opt s lets with
    | None ->
    begin match R.find_opt env s with
    | None -> raise (Typing_error("unbound variable",ast))
    | Some id ->
      apply (const id) (List.map (parse_formula env lets) l)
    end
    | Some t -> apply t (List.map (parse_formula env lets) l)
    end

  | { term = Ast.Binder (_,[],t); _; } ->
    parse_formula env lets t

  (* Local bindings *)
  | { Ast.term = Ast.Binder (Ast.Let, vars, f) } ->
    let rec aux lets = function
      | [] -> parse_formula env lets f
      | {Ast.term = Ast.Colon({Ast.term = Ast.Symbol s},t)}::l ->
        let t = parse_formula env lets t in
        if false then
          let s' = Format.asprintf "%a" Dolmen.Id.print s in
          let id = Witan_core.Id.mk s' t.Term.ty in
          R.add env s id;
          let l = aux lets l in
          R.remove env s;
          Term.letin id t l
        else
          let lets = MId.add s t lets in
          aux lets l
      | t::_ ->
        raise (Typing_error ("Unexpected let binding", t))
    in
    aux lets vars

  (* Functionnal arrows *)
  | { Ast.term = Ast.Binder (Ast.Arrow, vars, f) } ->
    Term.arrows (List.map (parse_formula env lets) vars) (parse_formula env lets f)

  (* Other cases *)
  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin _}, _) } ->
    raise (Typing_error ("Unexpected builtin", t))
  | { term = Ast.Builtin _; _; } ->
    raise (Typing_error ("Unexpected builtin", t))
  | { term = Ast.Colon (_,_); _; } ->
    raise (Typing_error ("Unexpected colon", t))
  | { term = Ast.App (_,_); _; }->
    raise (Typing_error ("Unexpected app", t))
  | { term = Ast.Binder (_,_,_); _; } ->
    raise (Typing_error ("Unexpected binder", t))
  | { term = Ast.Match (_,_); _; } ->
    raise (Typing_error ("Unexpected construction", t))

and parse_formula (env:env) lets (t:Dolmen.Term.t) =
  try
    parse_formula' env lets t
  with
  | (Typing_error _) as exn -> raise exn
  | exn ->
    raise (Typing_error (Printexc.to_string exn, t))


let get_loc =
  let default_loc = Dolmen.ParseLocation.mk "<?>" 0 0 0 0 in
  (fun t -> CCOpt.get_or ~default:default_loc t.Dolmen.Term.loc)

(** used to complete partial model *)
let get_model env d =
  let model : Value.t Term.H.t = Term.H.create 16 in
  R.iter (fun _ id ->
      let t = Term.const id in
      let n = SynTerm.node_of_term t in
      let v = Interp.model d n in
      Term.H.add_new Std.Impossible model t v)
    env;
  model

let interp_model model n =
  let leaf t = Term.H.find_opt model t in
  (Interp.node ~leaf n)

let check_model model expected n =
 Value.equal (interp_model model n) expected

let run ?limit ~theories statements =
  let env = create_env () in
  let clauses = ref [] in
  let open Witan_core in
  let res =
    Scheduler.run
      ~theories
      ?limit
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
               let ty = parse_formula env MId.empty t in
               let t' =
                 let s = Format.asprintf "%a" Dolmen.Id.print id in
                 Witan_core.Id.mk s ty
               in
               R.add_new Witan_stdlib.Std.Impossible env id t';
             | Clause l ->
               let map t = SynTerm.node_of_term (parse_formula env MId.empty t), Witan_core.Conflict.Pos in
               let l = Witan_stdlib.Shuffle.shufflel l in
               let l = List.map map l in
               let l = Witan_stdlib.Shuffle.shufflel l in
               let cl = !Witan_core.Conflict._or l in
               clauses := cl::!clauses;
               Egraph.register d cl;
               !Witan_core.Conflict._set_true d Trail.pexp_fact cl
             | Antecedent t ->
               let map t =
                 match parse_formula env MId.empty t with
                 | exception (Typing_error (msg, t)) ->
                   Format.eprintf
                     "%a:@\n%s:@ %a"
                     Dolmen.ParseLocation.fmt (get_loc t) msg
                     Dolmen.Term.print t;
                   Pervasives.exit 2
                 | t ->
                   SynTerm.node_of_term t
               in
               let t = Dolmen.Normalize.smtlib t in
               let cl = map t in
               clauses := cl::!clauses;
               Egraph.register d cl;
               !Witan_core.Conflict._set_true d Trail.pexp_fact cl
             | _ -> invalid_arg (Format.asprintf "Unimplemented command: %a" Dolmen.Statement.print stmt))
           statements) in
  match res with
  | `Contradiction -> `Unsat
  | `Done _d ->
    (* let model = get_model env d in
     * Format.printf "(%a)@."
     *   Witan_popop_lib.Pp.(iter22 Witan_core.Term.H.iter space
     *                         (fun fmt t v -> Format.fprintf fmt "(%a %a)"
     *                             Witan_core.Term.pp t Witan_core.Values.pp v))
     *   model *)
    `Sat



let () = Exn_printer.register (fun fmt exn ->
    match exn with
    | Typing_error (msg, t) ->
      Format.fprintf fmt
        "%a:@\n%s:@ %a"
        Dolmen.ParseLocation.fmt (get_loc t) msg
        Dolmen.Term.print t;
    | exn -> raise exn
  )
