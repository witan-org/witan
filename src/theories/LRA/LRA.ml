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

(** This module use one domain and two semantic values. *)
open Witan_popop_lib
open Witan_core
open Witan_stdlib.Std

let debug = Debug.register_info_flag
  ~desc:"for the arithmetic theory"
  "LRA"

let real = ValueKind.create_key (module struct type t = Q.t let name = "Q" end)

module RealValue = ValueKind.Register(struct
    include Q
    let key = real
  end)

let cst' c = RealValue.index ~basename:(Format.asprintf "%aR" Q.pp c) c Term._Real
let cst c = RealValue.node (cst' c)

let debug_todo = debug
type bound = Interval_sig.bound = Strict | Large
  [@@deriving eq,ord,show]
module S = struct
  module T = struct
    type t =
      | Add of Q.t * Node.t * Q.t * Node.t
      | GZero of Node.t * bound
      | Conflict of Polynome.t * bound
      [@@ deriving eq,ord]

    let pp fmt = function
      | Add (q1,cl1,q2,cl2) ->
        let pp fmt (q,node) =
          if Q.equal q Q.one then Node.pp fmt node else
          if Q.equal q Q.minus_one
          then Format.fprintf fmt "-%a" Node.pp node
          else Format.fprintf fmt "%a%a" Q.pp q Node.pp node
        in
        if Q.equal q2 Q.minus_one
        then Format.fprintf fmt "%a - %a" pp (q1,cl1) Node.pp cl2
        else Format.fprintf fmt "%a + %a" pp (q1,cl1) pp (q2,cl2)
      | GZero (node,b) ->
        Format.fprintf fmt "0 %a %a" Interval.pp_bound b Node.pp node
      | Conflict (p,b) ->
        Format.fprintf fmt "0 %a %a" Interval.pp_bound b Polynome.pp p

    let hash = function
      | Add (q1,cl1,q2,cl2) ->
        7 * (2*(Hashtbl.hash q1) + 3*Node.hash cl1
             + 5*(Hashtbl.hash q2) + 7*Node.hash cl2) + 1
      | GZero (node,Strict) -> 7 * Node.hash node + 2
      | GZero (node,Large) -> 7 * Node.hash node + 3
      | Conflict(p,Strict) -> CCHash.combine2 (Polynome.hash p) 4
      | Conflict(p,Large) -> CCHash.combine2 (Polynome.hash p) 5

  end
  include T
  include Stdlib.MkDatatype(T)
  let key = ThTermKind.create_key (module struct type nonrec t = t let name = "SARITH" end)
end

module SE = ThTermKind.Register(S)

module D = Interval.Convexe

let dom = Dom.create_key (module struct type t = D.t let name = "ARITH" end)

type exp =
  | ExpAdd of SE.t * Node.t (** on what we propagated *)
  | ExpEmptyDomMerge of Trail.Pexp.t * Node.t * Node.t
  | ExpEmptyDomInter of Trail.Pexp.t * Node.t
  | ExpDistIsZero of SE.t
  | ExpGZeroUp of SE.t * bool
  | ExpGZeroDown of SE.t * Node.t * bool
  | ExpIsSingleton of Trail.Pexp.t  * Node.t
                      * bool (* the domain of node *) * RealValue.t
  | ExpCst of RealValue.t
  | ExpDec of Node.t * Q.t
[@@ deriving show]

(** The explanation for a dom will always work on conflict which is an inequality *)

let exp = Trail.Exp.create_key (module struct type t = exp let name = "LRA.exp" end)

let set_dom d pexp node v b =
  match D.is_singleton v with
  | Some q ->
    let cst = cst' q in
    let pexp = Egraph.mk_pexp d exp (ExpIsSingleton(pexp,node,b,cst)) in
    Egraph.set_value d pexp node (RealValue.nodevalue cst)
  | None ->
    (** the pexp must be in the dom *)
    Egraph.set_dom d dom node v

let minus_or_one inv =
  if inv then Q.minus_one else Q.one

let print_bag_node = Bag.pp Format.(const char ',') Node.pp

let () = Dom.register(module struct
    include D
    let key = dom
    let merged i1 i2 =
      match i1, i2 with
      | None, None -> true
      | Some i1, Some i2 -> D.equal i1 i2
      | _ -> false

    let merge d pexp (i1,cl1) (i2,cl2) _ =
      assert (not (Egraph.is_equal d cl1 cl2));
      match i1, cl1, i2, cl2 with
      | Some i1,_, Some i2,_ ->
        begin match D.inter i1 i2 with
          | None ->
            let pexp = Egraph.mk_pexp d exp (ExpEmptyDomMerge(pexp,cl1,cl2)) in
            Egraph.contradiction d pexp
          | Some i ->
            if not (D.equal i i1) then
              Egraph.set_dom d dom cl1 i;
            if not (D.equal i i2) then
              Egraph.set_dom d dom cl2 i
        end
      | Some i1, _, _, cl2 | _, cl2, Some i1, _ ->
        Egraph.set_dom d dom cl2 i1
      | None,_,None,_ -> raise Impossible
  end)

module DaemonPropa = struct
  let key = Demon.Fast.create "LRA.DaemonPropa"

  module Data = SE

  let immediate = false
  let throttle = 100

  let gt_zero = D.gt Q.zero
  let ge_zero = D.ge Q.zero
  let lt_zero = D.lt Q.zero
  let le_zero = D.le Q.zero

  let get_dom del node = Opt.get_def D.reals (Egraph.get_dom del dom node)
  let get_value del node =
      match Egraph.get_value del real node with
        | None -> D.reals
        | Some d -> D.singleton d

  let upd del node d d' pexp =
    match D.inter d d' with
    | None ->
      let pexp = Egraph.mk_pexp del exp pexp in
      let pexp = Egraph.mk_pexp del exp (ExpEmptyDomInter(pexp,node)) in
      Debug.dprintf6 debug "[LRA] upd node = %a d = %a d' = %a"
        Node.pp node D.pp d D.pp d';
      Egraph.contradiction del pexp
    | Some d' ->
      if not (D.equal d d')
      then set_dom del (Egraph.mk_pexp del exp pexp) node d'
          (Equal.option Q.equal (D.is_singleton d') None)

  let upd_value del node d d' pexp =
    match D.inter d d' with
    | None ->
      let pexp = Egraph.mk_pexp del exp pexp in
      let pexp = Egraph.mk_pexp del exp (ExpEmptyDomInter(pexp,node)) in
      Debug.dprintf6 debug "[LRA] upd node = %a d = %a d' = %a"
        Node.pp node D.pp d D.pp d';
      Egraph.contradiction del pexp
    | Some d' ->
      if not (D.equal d d')
      then set_dom del (Egraph.mk_pexp del exp pexp) node d'
          (Equal.option Q.equal (D.is_singleton d') None)

  let propagate del s =
    match SE.sem s with
    | S.Add(q1,cl1,q2,cl2) ->
      let cl0 = SE.node s in
      let d0 = get_value del cl0 in
      if Q.equal q1 Q.one && Q.equal q2 Q.minus_one &&
         D.equal d0 D.zero then
        let pexp = Egraph.mk_pexp del exp (ExpDistIsZero(s)) in
        Egraph.merge del pexp cl1 cl2
      else
        let d1 = get_value del cl1 in
        let d2 = get_value del cl2 in
        let upd_value node d d' = upd_value del node d d' (ExpAdd(s,node)) in
        let qd1 = D.mult_cst q1 d1 in
        let qd2 = D.mult_cst q2 d2 in
        upd_value cl0 d0 (D.add qd1 qd2);
        upd_value cl1 d1 (D.mult_cst (Q.inv q1) (D.minus d0 qd2));
        upd_value cl2 d2 (D.mult_cst (Q.inv q2) (D.minus d0 qd1))
    | S.GZero(node,b) -> begin
        let cl0 = SE.node s in
        let d = get_value del node in
        let dzero_true  = if equal_bound b Strict then gt_zero else ge_zero in
        let dzero_false = if equal_bound b Strict then le_zero else lt_zero in
        if D.is_included d dzero_true
        then begin
          let pexp = Egraph.mk_pexp del exp (ExpGZeroUp(s,true)) in
          Boolean.set_true del pexp cl0
        end
        else if D.is_included d dzero_false
        then
          let pexp = Egraph.mk_pexp del exp (ExpGZeroUp(s,false)) in
          Boolean.set_false del pexp cl0
      end
    | S.Conflict(p,b) ->
      (** Choose representative of the equivalence class among the
            present classes, not the current representative *)
      let repr = Polynome.fold (fun acc node _ ->
          Node.M.add (Egraph.find del node) node acc)
          Node.M.empty p in
      let p' = Polynome.fold (fun acc node q ->
          let node = Egraph.find del node in
          let node = Node.M.find_exn Impossible node repr in
          Polynome.add acc (Polynome.monome q node)
        )
          (Polynome.cst p.cst)
          p in
      let rec aux d_first = function
        | [] -> begin
            let cl0 = SE.node s in
            if Equal.option Q.equal (D.is_singleton d_first) None then
              let d = d_first in
              let dzero_true  = if equal_bound b Strict then gt_zero else ge_zero in
              let dzero_false = if equal_bound b Strict then le_zero else lt_zero in
              if D.is_included d dzero_true
              then begin
                let pexp = Egraph.mk_pexp del exp (ExpGZeroUp(s,true)) in
                Boolean.set_true del pexp cl0;
                raise Exit
              end
              else if D.is_included d dzero_false
              then
                let pexp = Egraph.mk_pexp del exp (ExpGZeroUp(s,false)) in
                Boolean.set_false del pexp cl0;
                raise Exit
              else
                assert false
            else
              match Boolean.is del cl0 with
              | Some nonot ->
                let dzero = if equal_bound b Strict
                  then if nonot then gt_zero else le_zero
                  else if nonot then ge_zero else lt_zero in
                dzero,nonot
              | None ->
                raise Exit
        end
        | (node,q)::l ->
          let d = get_dom del node in
          let d' = (D.mult_cst q d) in
          let d_last,b = aux (D.add d' d_first) l in
          Debug.dprintf6 debug "node=%a d_first=%a d_last=%a"
            Node.pp node D.pp d_first D.pp d_last;
          let upd node d d' = upd del node d d' (ExpGZeroDown(s,node,b)) in
          upd node d (D.mult_cst (Q.inv q) (D.minus d_last d_first));
          D.minus d_last d', b
      in
      try
        ignore (aux (D.singleton p'.cst) (Node.M.bindings p'.poly))
      with Exit -> ()

  let wakeup del = function
    | Events.Fired.EventValue(_,_,s)
    | Events.Fired.EventDom(_,_,s) ->
      propagate del s
    | Events.Fired.EventChange(_,s) ->
      propagate del s
    | _ -> raise UnwaitedEvent

  let init del s =
    begin match SE.sem s with
      | S.Add (_,cl1,_,cl2) ->
    Debug.dprintf2 debug "TOTO: %a" SE.pp s;
        Egraph.register del cl1; Egraph.register del cl2;
        Demon.Fast.attach del key
          [Demon.Create.EventValue(SE.node s, real, s);
           Demon.Create.EventValue(cl1, real, s);
           Demon.Create.EventValue(cl2, real, s);
          ]
      | GZero (node,_) ->
        Egraph.register del node;
        Demon.Fast.attach del key
          [Demon.Create.EventValue(SE.node s, Boolean.dom, s);
           Demon.Create.EventValue(node, real, s)]
      | Conflict(p,_) ->
        Demon.Fast.attach del key
          [Demon.Create.EventValue(SE.node s, Boolean.dom, s)];
        Polynome.iter (fun node _ ->
            Egraph.register del node;
            Demon.Fast.attach del key
              [Demon.Create.EventValue(node, real, s);
               Demon.Create.EventChange(node, s);
              ]
          ) p
    end;
    propagate del s;
end

module RDaemonPropa = Demon.Fast.Register(DaemonPropa)

let zero = cst Q.zero
let one = cst Q.one
let index s = SE.index s Term._Real

let add' q1 cl1 q2 cl2 =
  let norm q node = if Q.equal q Q.zero then Q.one, zero else q, node in
  let q1, cl1 = norm q1 cl1 in
  let q2, cl2 = norm q2 cl2 in
  if Q.leq q2 q1 then
    index (S.Add(q1,cl1,q2,cl2))
  else
    index (S.Add(q2,cl2,q1,cl1))

let of_poly p =
  let m, c = Polynome.get_tree p in
  let rec aux = function
    | Polynome.Empty -> `None
    | Node(left,node,q,right,_) ->
      let left = aux left in
      let right = aux right in
      let r =
        match left, right with
        | `Some (lq,l), `Some (rq,r) ->
          `Some(Q.one,SE.node (add' lq l rq r))
        | `None, r | r, `None -> r
      in
      match r with
      | `None -> `Some(q,node)
      | `Some(rq,r) -> `Some(Q.one,SE.node (add' q node rq r))
  in
  match aux m with
  | `None -> cst c
  | `Some(rq,r) when Q.equal rq Q.one && Q.equal c Q.zero -> r
  | `Some(rq,r) -> SE.node (add' rq r c one)

let to_poly = function
  | S.Add(q1,cl1,q2,cl2) -> Polynome.of_list Q.one [cl1,q1;cl2,q2]
  | Conflict (p,_) -> p
  | GZero _ -> raise Impossible

let choarith =
  Trail.Cho.create_key (module struct type t = Node.t let name = "LRA.cho" end)

let make_dec node = Trail.GCho(node,choarith,node)

(** Choice *)
(*
(** Conflict *)
(** Reason of equalities between arithmetical terms
    exp: with all the decisions and propagation applied
    imp: without any decision and propagation applied
*)
type conpoly = {imp : Polynome.t; exp : Polynome.t; bound: bound;
                deps: Deps.t [@printer (fun _ _ -> ())]}
let pp_conpoly fmt x =
  Format.fprintf fmt "0 %s@ %a@ (%a)"
    (match x.bound with | Strict -> "<" | Large -> "<=")
    Polynome.pp x.imp
    Polynome.pp x.exp
let pp_conpoly' fmt x =
  Format.fprintf fmt "%a@ (%a)@ %s 0"
    Polynome.pp x.imp
    Polynome.pp x.exp
    (match x.bound with | Strict -> "<" | Large -> "<=")

type conpair = {mi: conpoly option; ma:conpoly option}
(** used as [0 <= x + P = mi /\ x + P = ma <= 0] *)
let pp_conpair fmt = function
  | {mi=None; ma=None} -> Format.fprintf fmt "None"
  | {mi=Some mi;ma=None} -> pp_conpoly fmt mi
  | {mi=None;ma=Some ma} -> pp_conpoly' fmt ma
  | {mi=Some mi;ma=Some ma} ->
    Format.fprintf fmt "%a@,⋀ %a" pp_conpoly mi pp_conpoly' ma

let interp_conpoly d p =
  let acc = Node.M.fold_left (fun acc node q ->
      let v = Opt.get_def D.reals (Egraph.get_dom d dom node) in
      D.add (D.mult_cst q v) acc
    ) D.zero p.imp.Polynome.poly in
  let acc = D.add_cst p.imp.cst acc in
  let good =
    if p.bound = Strict then DaemonPropa.gt_zero
    else DaemonPropa.ge_zero
  in
  match D.inter acc good with
  | None -> Conflict.False
  | Some i when D.equal i acc -> Conflict.True
  | Some _ -> Conflict.ToDecide

let condom : conpair Trail.con = Trail.Con.create_key "LRA.dom"

(** Return the corresponding bound *)
let get_exp_conpoly {exp={Polynome.cst}} = Q.neg cst

let mk_conpoly p = {imp = p; exp = p; bound=Large; deps = Deps.empty}
let mk_conpair p = let p = mk_conpoly p in {mi = Some p; ma = Some p}
let zero_conpoly = mk_conpoly Polynome.zero
let zero_conpair = mk_conpair Polynome.zero

let add_bound b1 b2 =
  match b1, b2 with
  | Large, Large -> Large
  | Strict, _ | _, Strict -> Strict

let switch q b1 b2 =
  if Q.leq Q.zero q then b1 else b2

let inv_bound = function
  | Large -> Strict
  | Strict -> Large

let add_conpoly p1 p2 =
  if p2 == zero_conpoly then p1
  else if p1 == zero_conpoly then p2
  else
    { imp = Polynome.add p1.imp p2.imp;
      exp = Polynome.add p1.exp p2.exp;
      bound = add_bound p1.bound p2.bound;
      deps = Deps.concat p1.deps p2.deps}

let add_conpair p1 p2 =
  {mi = Opt.map2 add_conpoly p1.mi p2.mi;
   ma = Opt.map2 add_conpoly p1.ma p2.ma}

let conpair_is_an_equality p1 =
  match p1.mi, p1.ma with
  | Some mi, Some ma ->
    Polynome.equal mi.exp ma.exp &&
    Q.equal mi.exp.cst Q.zero
  | _ -> false

let x_p_cy_conpoly p1 q p2 =
  if p2 == zero_conpoly then p1
  else
    {imp = Polynome.x_p_cy p1.imp q p2.imp;
     exp = Polynome.x_p_cy p1.exp q p2.exp;
     bound = add_bound p1.bound p2.bound;
     deps = Deps.concat p1.deps p2.deps
    }

let cx_p_cy_conpoly q1 p1 q2 p2 =
  {imp = Polynome.cx_p_cy q1 p1.imp q2 p2.imp;
   exp = Polynome.cx_p_cy q1 p1.exp q2 p2.exp;
   bound = add_bound p1.bound p2.bound;
   deps = Deps.concat p1.deps p2.deps;
  }

let cst_mult_conpoly q p =
  {imp = Polynome.mult_cst q p.imp;
   exp = Polynome.mult_cst q p.exp;
   bound = p.bound;
   deps = p.deps;
  }

let cst_mult_conpair q p =
  {mi = Opt.map (cst_mult_conpoly q) (switch q p.mi p.ma);
   ma = Opt.map (cst_mult_conpoly q) (switch q p.ma p.mi);
  }

let x_p_cy_conpair p1 q p2 =
  {mi = Opt.map2 (fun x y -> x_p_cy_conpoly x q y)
       p1.mi (switch q p2.mi p2.ma);
   ma = Opt.map2 (fun x y -> x_p_cy_conpoly x q y)
       p1.ma (switch q p2.ma p2.mi);
  }

let cx_p_cy_conpair q1 p1 q2 p2 =
  {mi = Opt.map2 (fun x y -> cx_p_cy_conpoly q1 x q2 y)
       (switch q1 p1.mi p1.ma)
       (switch q2 p2.mi p2.ma);
   ma = Opt.map2 (fun x y -> cx_p_cy_conpoly q1 x q2 y)
       (switch q1 p1.ma p1.mi)
       (switch q2 p2.ma p2.mi);
  }

let implies q p =
  begin match q.mi, p.mi with
    | _, None -> true
    | None, _ -> false
    | Some q, Some p ->
      match Polynome.is_cst (Polynome.sub p.exp q.exp) with
      | None -> false
      | Some cst ->
        let c = Q.compare Q.zero cst in
        if c = 0 then
          not (p.bound = Strict) || q.bound = Strict
        else c < 0
  end
  &&
  begin match q.ma, p.ma with
    | _, None -> true
    | None, _ -> false
    | Some q, Some p ->
      match Polynome.is_cst (Polynome.sub p.exp q.exp) with
      | None -> false
      | Some cst ->
        let c = Q.compare Q.zero cst in
        if c = 0 then
          not (p.bound = Strict) || q.bound = Strict
        else c > 0
  end

(** cl1 -> cl2 *)
let dist cl1 cl2 =
  (* contrary of vectors: here AB = OA - OB
     It is more instuitive for the distance with a constant:
     0 <= node - c    node - d <= 0
  *)
  Polynome.of_list Q.zero [cl1,Q.one;cl2,Q.minus_one]

let dist_conpoly cl1 cl2 =
  mk_conpoly (dist cl1 cl2)

let dist_conpair cl1 cl2 =
  mk_conpair (dist cl1 cl2)

let print_conpoly fmt t =
  Format.fprintf fmt "{imp=%a;exp=%a}" Polynome.pp t.imp Polynome.pp t.exp

let get_rlist_conpair_deps t cl1 cl2 deps =
  let r,deps =
    Conflict.ComputeConflict.Equal.one_equal
    t ~from:cl1 ~to_:cl2 condom zero_conpair deps
  in
  (* Debug.dprintf8 debug "cl1=%a cl2=%a r=%a dist=%a" *)
  (*   Node.pp cl1 Node.pp cl2 pp_conpair r Polynome.pp (dist cl1 cl2); *)
  assert (conpair_is_an_equality r);
  assert (Polynome.equal (Opt.get r.mi).exp (dist cl1 cl2));
  r,deps

let get_rlist_conpair t cl1 cl2 =
  let r, deps = get_rlist_conpair_deps t cl1 cl2 Trail.Deps.empty in
  Conflict.ComputeConflict.add_deps t deps;
  r

(** Gen Equality and disequality *)
module GenEquality = struct
  open Conflict

  let equality t cl1 cl2 =
    (** cl1 -> cl2 *)
    let p = get_rlist_conpair t cl1 cl2 in
    assert (conpair_is_an_equality p);
    (* Debug.dprintf6 debug "cl1=%a cl2=%a p=%a" *)
    (*   Node.pp cl1 Node.pp cl2 pp_conpair p; *)
    (** cl2 -> cl1 *)
    let p = add_conpair p (dist_conpair cl2 cl1) in
    (** cl1 -> cl2 -> cl1 = 0 *)
    assert (conpair_is_an_equality p);
    assert (Polynome.is_zero (Opt.get p.mi).exp);
    Debug.dprintf6 debug "[LRA] %a=%a: %a" Node.pp cl1 Node.pp cl2 pp_conpair p;
    ComputeConflict.unknown_con t condom p

  let expspecial =
    { Equality.equality = equality;
      disequality = (fun t _age ~hyp:_ cl1d cl1e cl2e cl2d ->
          equality t cl1d cl1e;
          equality t cl2d cl2e);
      merged = (fun t deps _age cl1d cl1 pexp cl2 cl2d ->
          let eq_t = ComputeConflict.Equal.init condom
              zero_conpair deps ~from:cl1d in
          let eq_t = ComputeConflict.Equal.add_equal t eq_t ~to_:cl1 in
          let eq_t = ComputeConflict.Equal.add_pexp t eq_t ~to_:cl2 pexp in
          let eq_t = ComputeConflict.Equal.add_equal t eq_t ~to_:cl2d in
          let p,deps = ComputeConflict.Equal.close eq_t in
          (** cl2d -> cl1d *)
          let pd = dist_conpair cl2d cl1d in
          let p = add_conpair p pd in
          (* Debug.dprintf2 debug "sum: %a" pp_conpair p; *)
          Trail.Deps.add_unknown_con deps condom p);
      dodec = true (** TODO *);
      new_true_disequality = (fun _ _ _ -> ());
    }

  let () = Equality.register_sort Term._Real expspecial

end
*)

type hypbound =
  | Eq
  | Le
  | Lt
[@@deriving eq]

let _ = Eq
let _ = Le
let _ = Lt

type hyppoly = {
  bound: hypbound ;
  poly: Polynome.t ;
}

let pp_hyppoly fmt c =
  let bound = function
    | Eq -> "="
    | Le -> "󠀼≤"
    | Lt -> "<"
  in
  Format.fprintf fmt "0 %s %a"
    (bound c.bound)
    Polynome.pp c.poly

module HypDom = struct
  type t = hyppoly

  let pp = pp_hyppoly

  let key =
    Trail.Hyp.create_key (module struct type nonrec t = t let name = "Arith.hyp" end)

  let pp_v fmt v =
    let aux fmt (_,v) = pp_hyppoly fmt v in
    SE.M.bindings v |> Format.(list ~sep:(const char ';') aux) fmt

  let levels _ = assert false
  let split _ = assert false
  let apply_learnt hyp =
    match hyp.bound with
    | Eq ->
      let n = of_poly hyp.poly in
      (Equality.equality [n;zero], Conflict.Neg)
    | Le | Lt ->
      let b = if equal_hypbound hyp.bound Le then Interval.Large else Interval.Strict in
      let i = SE.index (S.Conflict(hyp.poly, b)) Term._Real in
      let i = SE.node i in
      (i, Conflict.Neg)
  let useful_nodes _ = assert false
end

let () = Conflict.register_hyp(module HypDom)

module ExpEquality = struct
  (* open Conflict *)

  type t = exp
  let pp = pp_exp
  let key = exp

  (* let extract_add s node = match SE.sem s with
   *   | S.Cst _
   *   | S.GZero _
   *   | S.Conflict _
   *     -> raise Impossible
   *   (\* cl1 = 1/q1*(SE.node s) - q2/q1*cl2 *\)
   *   | S.Add (q1,cl1,q2,cl2) when Node.equal node cl1 ->
   *     cl1, (Q.inv q1), SE.node s, Q.neg (Q.div q2 q1), cl2
   *   (\* cl2 = 1/q2*(SE.node s) - q1/q2*cl1 *\)
   *   | S.Add (q1,cl1,q2,cl2) when Node.equal node cl2 ->
   *     cl2, (Q.inv q2), SE.node s, Q.neg (Q.div q1 q2), cl1
   *   (\* SE.node s = q1*cl1 + q2*cl2 *\)
   *   | S.Add (q1,cl1,q2,cl2) ->
   *     SE.node s,q1,cl1,q2,cl2
   * 
   * (\** the result must be physically one or the other *\)
   * let best_bound_inf op1 op2 =
   *   match op1,op2 with
   *   | None, None -> None
   *   | None, (Some _ as p) | (Some _ as p), None -> p
   *   | Some p1, Some p2 ->
   *     let q1 = get_exp_conpoly p1 in
   *     let q2 = get_exp_conpoly p2 in
   *     if Interval.compare_bounds_inf (q1,p1.bound) (q2,p2.bound) < 0
   *     then op2 else op1
   * 
   * (\** the result must be physically one or the other *\)
   * let best_bound_sup op1 op2 =
   *   match op1,op2 with
   *   | None, None -> None
   *   | None, (Some _ as p) | (Some _ as p), None -> p
   *   | Some p1, Some p2 ->
   *     let q1 = get_exp_conpoly p1 in
   *     let q2 = get_exp_conpoly p2 in
   *     if Interval.compare_bounds_sup (q1,p1.bound) (q2,p2.bound) < 0
   *     then op1 else op2
   * 
   * let best_bound p1 p2 =
   *   { mi = best_bound_inf p1.mi p2.mi;
   *     ma = best_bound_sup p1.ma p2.ma }
   * 
   * (\**
   *    0 <= x + P     x + Q < 0
   *    implies
   *      0 < x - x + P - Q      ( -P <= x < -Q )
   *    there was an empty domain so that it was not verified. So for the proof
   *    we suppose that it is not verified
   *    0 <= Q - P
   * *\)
   * let bound_distance_not_verified p1 =
   *   { mi =
   *       Opt.map2 (fun mi ma ->
   *           let p = x_p_cy_conpoly ma Q.minus_one mi in
   *           { p  with bound = inv_bound p.bound }
   *         ) p1.mi p1.ma;
   *     ma = None }
   * 
   * let get_pexp_or_add_def t pexp =
   *   match Conflict.Helpers.get_pexp_or_add t pexp condom with
   *   | None -> assert false
   *   | Some p -> p
   * 
   * let get_dom t age node =
   *   (\* Look at all the modifications of the cls that are part of the
   *      equivalence class of this node, and keep the two with the best bounds
   *   *\)
   *   Debug.dprintf2 ~nobox:() debug "@,@[<v 3>@[[LRA] get_dom for %a@]" Node.pp node;
   *   let f t =
   *     let mod_doms = get_dom_before_last_dec t age node dom in
   *     let mi, ma =
   *       List.fold_left
   *         (fun (((mi,_) as miacc),((ma,_) as maacc))
   *           (mod_dom:Trail.mod_dom) ->
   *           (\** dom -> modcl *\)
   *           let p' = (get_pexp_or_add_def t mod_dom.modpexp) in
   *           let mi'' = best_bound_inf mi p'.mi in
   *           let ma'' = best_bound_sup ma p'.ma in
   *           (if mi'' == mi then miacc else (p'.mi,mod_dom.modcl)),
   *           (if ma'' == ma then maacc else (p'.ma,mod_dom.modcl)))
   *         ((None,(\* dumb *\) zero),(None,(\* dumb *\) zero))
   *         mod_doms in
   *     let f which = function
   *       | (None,_) ->
   *         Debug.dprintf0 debug "[LRA] Choose None";
   *         None
   *       | (d,modcl) ->
   *         (\** node -> modcl *\)
   *         Debug.dprintf4 debug "[LRA] Choose %a from %a" (Opt.pp pp_conpoly) d Node.pp modcl;
   *         Opt.map2 add_conpoly d (which (get_rlist_conpair t node modcl))
   *     in
   *     { mi = f (fun c -> c.mi) mi; ma = f (fun c -> c.ma) ma }
   *   in
   *   let p,deps = ComputeConflict.wrap_deps t f in
   *   let add_deps m = {m with deps = Deps.concat deps m.deps } in
   *   Debug.dprintf0 ~nobox:() debug "@]";
   *   { mi = Opt.map add_deps p.mi; ma = Opt.map add_deps p.ma }
   * 
   * 
   * let analyse t age con = function
   *   | ExpCst(node,q) ->
   *     Conflict.return con condom
   *       (mk_conpair (Polynome.of_list (Q.neg q) [node,Q.one]))
   *   | ExpAdd (s,cls) -> begin
   *     match SE.sem s with
   *     | S.Add _->
   *       let cl0, q1, cl1, q2, cl2 = extract_add s cls in
   *       let d1 = get_dom t age cl1 in
   *       (\* Debug.dprintf2 debug "[LRA] d1=%a" pp_conpair d1; *\)
   *       let d2 = get_dom t age cl2 in
   *       (\* Debug.dprintf2 debug "[LRA] d2=%a" pp_conpair d2; *\)
   *       let semv = mk_conpair
   *           (Polynome.of_list Q.zero [cl0,Q.one;cl1,Q.neg q1;cl2,Q.neg q2]) in
   *       let d0 = add_conpair semv (cx_p_cy_conpair q1 d1 q2 d2) in
   *       (\* Debug.dprintf2 debug "[LRA] d0=%a" pp_conpair d0; *\)
   *       (\* Debug.dprintf10 debug *\)
   *       (\*   "[LRA] cl0=%a q1=%a cl1=%a q2=%a cl2=%a" *\)
   *       (\* Node.pp cl0 Q.pp q1 Node.pp cl1 Q.pp q2 Node.pp cl2 ; *\)
   *       Conflict.return con condom d0
   *     | S.Conflict p ->
   *       let repr = Polynome.fold (fun acc node _ ->
   *           Node.M.add (Conflict.ComputeConflict.get_repr_at t age node) node acc)
   *           Node.M.empty p in
   *       let cl0 = SE.node s in
   *       let semv,p' = Polynome.fold (fun (semv,acc) node q ->
   *           let node' = Conflict.ComputeConflict.get_repr_at t age node in
   *           let node' = Node.M.find_exn Impossible node' repr in
   *           let semv = x_p_cy_conpair semv q (get_rlist_conpair t node' node) in
   *           semv,Polynome.add acc (Polynome.monome q node')
   *         )
   *           (mk_conpair p,Polynome.cst p.cst)
   *           p in
   *       let pcl0 = Polynome.monome Q.minus_one cl0 in
   *       let semv = add_conpair semv (mk_conpair pcl0) in
   *       let p' = Polynome.add p' pcl0 in
   *       Debug.dprintf2 debug "[LRA] p=%a" Polynome.pp p;
   *       Debug.dprintf2 debug "[LRA] semv=%a" pp_conpair semv;
   *       Debug.dprintf2 debug "[LRA] p'=%a" Polynome.pp p';
   *       let mi = match semv.mi with
   *         | None -> assert false
   *         | Some mi -> mi in
   *       assert (Polynome.equal p' mi.exp);
   *       let qcls = Node.M.find_exn Impossible cls (mi.exp).poly in
   *       let semv = cst_mult_conpair (Q.inv qcls) semv in
   *       let mi = match semv.mi with
   *         | None -> assert false
   *         | Some mi -> mi in
   *       let semv =
   *         Polynome.fold (fun semv node q ->
   *             if Node.equal node cls then semv
   *             else
   *               let d = get_dom t age node in
   *               x_p_cy_conpair semv (Q.neg q) d
   *           ) semv (mi.exp) in
   *       Conflict.return con condom semv
   *     | _ -> raise Impossible
   *     end
   *   | ExpGZeroDown (s,nonot) ->
   *     let node,b = match SE.sem s with
   *       | S.GZero (node,b) -> node,b
   *       | _ -> raise Impossible in
   *     let cl0 = SE.node s in
   *     ComputeConflict.unknown_con t conclause
   *       (Boolean.get_dom t age cl0 Node.M.empty);
   *     let p =
   *       let exp = Polynome.monome Q.one node in
   *       if nonot
   *       then
   *         {mi = Some
   *              { imp = exp; exp; bound=b;
   *                deps = Deps.empty};
   *          ma = None}
   *       else
   *         {ma = Some
   *              { imp = exp; exp;
   *                bound=inv_bound b;
   *                deps = Deps.empty};
   *          mi = None}
   *     in
   *     Conflict.return con condom p
   *   | ExpEmptyDomMerge (pexp,cl1,cl2) ->
   *     let d1 = get_dom t age cl1 in
   *     let d2 = get_dom t age cl2 in
   *     let eq,deps =
   *       Conflict.ComputeConflict.Equal.one_pexp t ~from:cl1 ~to_:cl2 condom
   *         zero_conpair Deps.empty pexp
   *     in
   *     Conflict.ComputeConflict.add_deps t deps;
   *     assert (conpair_is_an_equality eq);
   *     Debug.dprintf6 debug "d1=%a;@ d2=%a;@ eq=%a"
   *       pp_conpair d1 pp_conpair d2 pp_conpair eq;
   *     let d2 = add_conpair eq d2 in
   *     let r = best_bound d1 d2 in
   *     Debug.dprintf4 debug "d2=%a@ r=%a"
   *       pp_conpair d2 pp_conpair r;
   *     let r = bound_distance_not_verified r in
   *     assert (None <> r.mi);
   *     assert (match Polynome.is_cst (Opt.get r.mi).exp,
   *                   (Opt.get r.mi).bound with
   *            | Some q, Strict -> Q.lt Q.zero q
   *            | Some q, Large  -> Q.leq Q.zero q
   *            | None,_  -> false);
   *     return con condom r
   *   | ExpEmptyDomInter (pexp,cl1) ->
   *     let d1 = get_dom t age cl1 in
   *     Debug.dprintf2 debug "d1=%a" pp_conpair d1;
   *     let d2 = (get_pexp_or_add_def t pexp) in
   *     Debug.dprintf2 debug "d2=%a" pp_conpair d2;
   *     let r' = best_bound d1 d2 in
   *     let r = bound_distance_not_verified r' in
   *     Debug.dprintf4 debug "r'=%a r=%a@"
   *       pp_conpair r' pp_conpair r;
   *     assert (None <> r.mi);
   *     assert (match Polynome.is_cst (Opt.get r.mi).exp,
   *                   (Opt.get r.mi).bound with
   *            | Some q, Strict -> Q.lt Q.zero q
   *            | Some q, Large  -> Q.leq Q.zero q
   *            | None,_  -> false);
   *     return con condom r
   *   | ExpGZeroUp(s,nonot) ->
   *     let node,b = match SE.sem s with
   *       | S.GZero (node,b) -> node,b
   *       | _ -> raise Impossible in
   *     let d = get_dom t age node in
   *     Debug.dprintf6 debug "node=%a %a d=%a" Node.pp node pp_bound b pp_conpair d;
   *     if nonot then begin
   *       assert ( implies d {ma = None; mi = Some { (mk_conpoly (Polynome.monome Q.one node)) with bound = b}} );
   *       ComputeConflict.unknown_con t condom
   *         { d with ma = None }
   *     end else  begin
   *       assert ( implies d {mi = None; ma = Some { (mk_conpoly (Polynome.monome Q.one node))
   *                                                  with bound = inv_bound b}} );
   *       ComputeConflict.unknown_con t condom
   *         { d with mi = None }
   *     end;
   *     Conflict.return con conclause Node.M.empty
   *   | ExpDistIsZero s ->
   *     let cl0, q1, cl1, q2, cl2 = extract_add s (SE.node s) in
   *     let d0 = get_dom t age cl0 in
   *     let semv = mk_conpair
   *         (Polynome.of_list Q.zero [cl0,Q.minus_one;cl1,q1;cl2,q2]) in
   *     return con condom (add_conpair semv d0)
   *   | ExpIsSingleton(pexp,node,b,cst) ->
   *     let q = match SE.sem cst with | Cst q -> q | _ -> raise Impossible in
   *     let d1 = if b then get_dom t age node else {mi=None;ma=None} in
   *     let d2 = (get_pexp_or_add_def t pexp) in
   *     let r = best_bound d1 d2 in
   *     Debug.dprintf8
   *       debug
   *       "r=%a d1=%a d2=%a q=%a"
   *       pp_conpair r pp_conpair d1 pp_conpair d2 Q.pp q;
   *     Conflict.return con condom r
   * 
   * let expdomlimit _t _age dom' node con v _ =
   *   let v = Opt.get_exn Impossible v in
   *   let v = Dom.Eq.coerce dom' dom v in
   *   let mk = function
   *     | None -> None
   *     | Some (v,bound) ->
   *       let p = Polynome.of_list (Q.neg v) [node,Q.one] in
   *       Some {imp = p; exp = p; bound; deps = Deps.empty} in
   *   let mi,ma = D.get_convexe_hull v in
   *   return con condom {mi=mk mi;ma=mk ma}
   * 
   * 
   * let same_sem (type a) t age (sem':a sem) (v:a) con exp cl1 cl2 =
   *   let r1 = analyse t age condom exp in
   *   let p = match r1 with
   *     | GRequested p1 ->
   *       let p2 =
   *         match Sem.Eq.eq_type S.key sem' with
   *         | None -> raise Impossible (\* understand why that happend *\)
   *         | Some Types.Eq ->
   *           Polynome.x_p_cy (Polynome.monome Q.one cl2) Q.minus_one
   *               (to_poly v)
   *       in
   *       x_p_cy_conpair p1 Q.minus_one (mk_conpair p2)
   *     | GOther _ -> raise Impossible (\* not created by analyse *\)
   *   in
   *   Debug.dprintf6 debug_todo "@[same_sem cl1:%a cl2:%a = %a@]"
   *     Node.pp cl1 Node.pp cl2 pp_conpair p;
   *   assert (conpair_is_an_equality p);
   *   assert (Polynome.equal (Opt.get p.mi).exp (dist cl1 cl2));
   *   Conflict.return con condom p *)

  let analyse _ = assert false
  let from_contradiction _ = assert false

end

let () = Conflict.register_exp(module ExpEquality)


module ChoLRA = struct
  open Conflict

  module OnWhat = Node

  let make_decision node b env =
    Debug.dprintf4 print_decision
      "[LRA] decide %a on %a" Q.pp b Node.pp node;
    let pexp = Egraph.mk_pexp env exp (ExpDec(node,b)) in
    set_dom env pexp node (D.singleton b) false

  let choose_decision env node =
    let v = Opt.get_def D.reals (Egraph.get_dom env dom node) in
    match D.is_singleton v with
    | Some _ -> DecNo
    | None -> DecTodo (make_decision node (D.choose v))
  let key = choarith

end

let () = Conflict.register_cho(module ChoLRA)

(** API *)

let index x = SE.node (SE.index x Term._Real)

let as_node node = index (S.Add (Q.one,node,Q.one,zero))

let add' q1 cl1 q2 cl2 =
  SE.node (add' q1 cl1 q2 cl2)

let add cl1 cl2 =
  add' Q.one cl1 Q.one cl2

let sub cl1 cl2 =
  index (S.Add(Q.one,cl1,Q.minus_one,cl2))

let neg cl2 =
  index (S.Add(Q.one,zero,Q.minus_one,cl2))

let mult _cl1 _cl2 = raise (TODO "mult without constant")

let mult_cst cst node =
  add' cst node Q.one zero

let gt_zero node =
  SE.node (SE.index (GZero(node,Strict)) Boolean.ty)

let ge_zero node =
  SE.node (SE.index (GZero(node,Large)) Boolean.ty)

let lt cl1 cl2 = gt_zero (sub cl2 cl1)
let le cl1 cl2 = ge_zero (sub cl2 cl1)
let gt cl1 cl2 = lt cl2 cl1
let ge cl1 cl2 = le cl2 cl1

(** {2 Initialization} *)
let converter d f l =
  let of_term t =
    let n = SynTerm.node_of_term t in
    Egraph.register d n;
    n
  in
  let node = match f, l with
    | f,[] when Term.is_const_real_term f ->
      Some (cst (Term.get_const_real_term f))
    | f,a::args when Term.is_add_real_term f ->
      Some (List.fold_left add (of_term a) (List.map of_term args))
    | f,[arg1;arg2] when Term.equal f Term.sub_real_term ->
      Some (sub (of_term arg1) (of_term arg2))
    | f,[arg] when Term.equal f Term.neg_real_term ->
      Some (neg (of_term arg))
    | f,args when Term.equal f Term.mul_real_term -> begin
        let mult_cst c t =
          Some (mult_cst (Term.get_const_real_term c) (of_term t))
        in
        match args with
        | [arg1;arg2] when Term.is_const_real_term arg1 ->
          mult_cst arg1 arg2
        | [arg1;arg2] when Term.is_const_real_term arg2 ->
          mult_cst arg2 arg1
        | _ -> None
      end
    | f,[arg1;arg2] when Term.is_lt_real_term f ->
      Some (lt (of_term arg1) (of_term arg2))
    | f,[arg1;arg2] when Term.is_le_real_term f ->
      Some (le (of_term arg1) (of_term arg2))
    | _ -> None in
  node

let decvars n =
  if Ty.equal (Node.ty n) Term._Real
  then Some (make_dec n)
  else None


let th_register env =
  RDaemonPropa.init env;
  Demon.Fast.register_init_daemon
    ~immediate:true
    ~name:"LRA.DaemonInit"
    (module SE)
    DaemonPropa.init
    env;
  SynTerm.register_converter env converter;
  SynTerm.register_decvars env decvars;
  Demon.Fast.register_init_daemon_value
    ~name:"RealValueToDom"
    (module RealValue)
    (fun d value ->
       let v = RealValue.value value in
       let s = D.singleton v in
       let _pexp = Egraph.mk_pexp d exp (ExpCst value) in
       (** something must be done with the pexp *)
       Egraph.set_dom d dom (RealValue.node value) s
    ) env;
  ()

(** {2 Interpretations} *)
let () =
  let gzero bound n =
    let v = (match bound with | Strict -> Q.lt | Large -> Q.le) Q.zero n in
    (if v then Boolean.values_true else Boolean.values_false)
  in
  let interp ~interp (t:S.t) =
    let get_v n = RealValue.value (RealValue.coerce_nodevalue (interp n)) in
    match t with
    | Add(q1,n1,q2,n2) ->
      let v = Q.( q1 * (get_v n1) + q2 * (get_v n2)) in
      RealValue.nodevalue (RealValue.index v Term._Real)
    | GZero(n,bound) ->
      gzero bound (get_v n)
    | Conflict (p,bound) ->
      let v = Polynome.fold (fun acc n q -> Q.( acc + q * (get_v n))) Q.zero p in
      gzero bound v
  in
  Interp.Register.thterm S.key interp

let default_value = Q.zero

let () =
  Interp.Register.model Term._Real (fun d n ->
      let v = Egraph.get_value d real n in
      let v = Witan_popop_lib.Opt.get_def default_value v in
      let v = RealValue.nodevalue (RealValue.index v Term._Real) in
      v)

let () =
  Interp.Register.id (fun id args ->
      let is builtin = Term.Id.equal id builtin in
      let (!>) n = RealValue.value (RealValue.coerce_nodevalue n) in
      let (!<) v = Some (RealValue.nodevalue (RealValue.index v Term._Real)) in
      let (!<<) b = Some (if b then Boolean.values_true else Boolean.values_false) in
      match args with
      | [] when Term.is_const_real_id id ->
        !< (Term.get_const_real_id id)
      | args when Term.is_add_real_id id ->
        !< (List.fold_left (fun acc a -> Q.add acc (!> a)) Q.zero args)
      | [arg1;arg2] when is Term.sub_real_id ->
        !< (Q.sub (!> arg1) (!> arg2))
      | [arg] when is Term.neg_real_id ->
        !< (Q.neg (!> arg))
      | [arg1;arg2] when is Term.mul_real_id ->
          !< (Q.mul (!> arg1) (!> arg2))
      | [arg1;arg2] when Term.is_lt_real_id id ->
        !<< (Q.lt (!> arg1) (!> arg2))
      | [arg1;arg2] when Term.is_le_real_id id ->
        !<< (Q.le (!> arg1) (!> arg2))
      | _ -> None
    )
