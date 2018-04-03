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
      | [a; b] ->
        let a = parse_formula env a in
        let b = parse_formula env b in
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

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Distinct}, args) } ->
    apply (distinct_term (List.length args)) (List.map (parse_formula env) args)

  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Ite}, l) } as t ->
    begin match l with
      | [cond;then_; else_] ->
        let cond  = parse_formula env cond in
        let then_ = parse_formula env then_ in
        let else_ = parse_formula env else_ in
        apply ite_term [then_.Term.ty;cond;then_;else_]
      | _ -> _bad_op_arity env "ite" 3 t
    end

  (* General case: application *)
  | { Ast.term = Ast.Symbol s }
  | { Ast.term = Ast.App ({ Ast.term = Ast.Symbol s }, []) } ->
    let id = R.memo (fun id ->
        let s = Format.asprintf "%a" Dolmen.Id.print id in
        (** only in dimacs they are not declared *)
        Witan_core.Id.mk s _Prop) env s in
    const id

  | { Ast.term = Ast.App ({ Ast.term = Ast.Symbol s }, l) } as ast ->
    begin match R.find_opt env s with
    | None -> raise (Typing_error("unbound variable",env,ast))
    | Some id ->
      apply (const id) (List.map (parse_formula env) l)
    end

  (* (\* Local bindings *\)
   * | { Ast.term = Ast.Binder (Ast.Let, vars, f) } ->
   *   parse_let env f vars *)

  | { term = Ast.Binder (_,[],t); _; } ->
    parse_formula env t

  (* Other cases *)
  | { Ast.term = Ast.App ({Ast.term = Ast.Builtin _}, _) } ->
    raise (Typing_error ("Unexpected builtin", env, t))
  | { term = Ast.Builtin _; _; } ->
    raise (Typing_error ("Unexpected builtin", env, t))
  | { term = Ast.Colon (_,_); _; } ->
    raise (Typing_error ("Unexpected colon", env, t))
  | { term = Ast.App (_,_); _; }->
    raise (Typing_error ("Unexpected app", env, t))
  | { term = Ast.Binder (_,_,_); _; } ->
    raise (Typing_error ("Unexpected binder", env, t))
  | { term = Ast.Match (_,_); _; } ->
    raise (Typing_error ("Unexpected construction", env, t))

let get_loc =
  let default_loc = Dolmen.ParseLocation.mk "<?>" 0 0 0 0 in
  (fun t -> CCOpt.get_or ~default:default_loc t.Dolmen.Term.loc)

(** used to complete partial model *)
let get_model env d =
  let model : Values.t Term.H.t = Term.H.create 16 in
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
 Values.equal (interp_model model n) expected
