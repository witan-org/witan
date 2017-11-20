(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2013                                                    *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Witan_core
open Std
open Typedef
open Stdlib
open Solver

let lazy_propagation = false

let debug = Debug.register_info_flag
  ~desc:"for the boolean theory"
  "bool"

let ty_ctr = Ty.Constr.create "Bool"
let ty = Ty.ctr ty_ctr
let dom : bool dom = Dom.create_key "bool"
let cl_true = Cl.fresh "⊤" ty
let cl_false = Cl.fresh "⊥" ty

let union_disjoint m1 m2 =
  Cl.M.union (fun _ b1 b2 -> assert (b1 == b2); Some b1) m1 m2

let index sem v = Cl.index sem v ty

let with_other = ref false
(** TODO not a global variable...
    merge the element that are true instead of using only the dom.
    false by default
*)


let is env cl = Solver.Delayed.get_dom env dom cl
let is_true  env cl = Cl.equal cl cl_true || is env cl = Some true
let is_false env cl = Cl.equal cl cl_false || is env cl = Some false
let is_unknown env cl = is env cl = None

module D = struct
  type t = bool
  let merged b1 b2 = Opt.equal (==) b1 b2

  type expmerge =
  | Merge of Explanation.pexp * Cl.t * Cl.t
  | DomMerge of Explanation.pexp * Cl.t

  let expmerge : expmerge Explanation.exp =
    Explanation.Exp.create_key "Bool.merge"

  let true_is_false d cl pexp =
    let pexp = Delayed.mk_pexp d expmerge (DomMerge(pexp,cl)) in
    Delayed.contradiction d pexp

  let set_bool env pexp cl b =
    if !with_other then
      Delayed.merge env pexp cl
        (if b then cl_true else cl_false)
    else
      match Delayed.get_dom env dom cl with
      | Some b' when DBool.equal b b' -> ()
      | Some _ ->
        true_is_false env cl pexp
      | None ->
        Delayed.set_dom env pexp dom cl b

  let merge d pexp ((b1: t option),cl1) ((b2: t option),cl2) _ =
      match b1,b2 with
      | Some b1, Some b2 when b1 == b2 -> ()
      | Some _, Some _  ->
        let pexp = Delayed.mk_pexp d expmerge (Merge(pexp,cl1,cl2)) in
        Delayed.contradiction d pexp
      | None, Some b ->
        Delayed.set_dom_premerge d dom cl1 b
      | Some b, None ->
        Delayed.set_dom_premerge d dom cl2 b
      | None, None -> assert false

  let pp = Pp.bool
  let key = dom
end

let true_is_false = D.true_is_false
let set_bool = D.set_bool
module DE = RegisterDom(D)

type t =
  { topnot: bool;
    lits: (Cl.t * bool) IArray.t;
  }

let sem : t sem = Sem.create_key "Prop"

(* let iter f x = IArray.iter f x.lits *)

let fold f acc x = IArray.fold f acc x.lits

let isnot v =
  if IArray.length v.lits == 1 then
    let cl,sign = IArray.get v.lits 0 in
    assert (v.topnot && not sign);
    Some cl
  else
    None

let mulbool b1 b2 = b1 != b2

module T = struct
  type r = t
  type t = r
  let equal n1 n2 =
    let clbool (cl1,b1) (cl2,b2) = Cl.equal cl1 cl2 && DBool.equal b1 b2 in
    n1.topnot == n2.topnot &&
    IArray.equal clbool n1.lits n2.lits

  let hash n =
    let clbool (cl,b) = Hashcons.combine (Cl.hash cl) (DBool.hash b) in
    Hashcons.combine (DBool.hash n.topnot) (IArray.hash clbool n.lits)

  let compare n1 n2 =
    let c = DBool.compare n1.topnot n2.topnot in
    if c != 0 then c else
      let clbool (cl1,b1) (cl2,b2) =
        let c = Cl.compare cl1 cl2 in
        if c != 0 then c
        else DBool.compare b1 b2 in
      IArray.compare clbool n1.lits n2.lits


  let print_cl fmt cl b =
    if b
    then Format.fprintf fmt "¬ %a" Cl.pp cl
    else Cl.pp fmt cl

  let pp fmt x =
    match isnot x with
    | Some cl ->
      print_cl fmt cl true
    | None ->
      let print_cl topnot fmt (cl,b) = print_cl fmt cl (mulbool topnot b) in
      if x.topnot
      then Format.fprintf fmt "⋀(%a)"
          (Pp.iter1 IArray.iter Pp.comma (print_cl true)) x.lits
      else Format.fprintf fmt "⋁(%a)"
          (Pp.iter1 IArray.iter Pp.comma (print_cl false)) x.lits

end

module Th = struct
  include T
  include MkDatatype(T)

  let key = sem

  exception TopKnown of bool

  exception Found of Cl.t * bool
  let find_not_known d l =
    IArray.iter (fun (cl,b) ->
      match Delayed.get_dom d dom cl with
      | Some _ -> ()
      | None -> raise (Found (cl,b))
    ) l

  let _bcp d l absorbent =
    try
      let res = IArray.fold (fun acc cl ->
        match Delayed.get_dom d dom cl, acc with
        | Some b, _ when b = absorbent -> raise (TopKnown absorbent)
        | Some _, _ -> acc
        | None, Some _ -> raise Exit
        | None, None -> Some cl)
        None l in
      match res with
      | None -> raise (TopKnown (not absorbent))
      | _ -> res
    with Exit -> None

end

module ThE = RegisterSem(Th)

type bcpkind =
  | BCPOwnKnown
  | BCPLeavesKnown
  | BCP

type expprop =
| ExpBCP  of ThE.t (* own *) * Cl.t (* propa *) * bcpkind
| ExpUp  of ThE.t (* own *) * Cl.t  (* one leaf to own *)
| ExpDown of ThE.t (* own *) * Cl.t (* leaf *)(* own to leaf *)
| ExpNot  of (Th.t * Cl.t * Cl.t)
(* | ExpNot  of Th.t * Cl.t * Cl.t (* t <> not t or t = not (not t) *) *)

let expprop : expprop Explanation.exp =
  Explanation.Exp.create_key "Bool.prop"

module DaemonPropaNot = struct

  module Data = struct
    type t = Th.t * Cl.t * Cl.t
    let pp fmt (v,cl1,cl2) =
      Format.fprintf fmt "%a,%a -> %a" Th.pp v Cl.pp cl1 Cl.pp cl2
  end

  let immediate = false
  let key = Demon.Fast.create "Bool.DaemonPropaNot"
  let throttle = 100
  let wakeup d =
    function
    | Events.Fired.EventDom(_,dom',((_,cl,ncl) as x)) ->
      assert (Dom.equal dom dom');
      begin match Delayed.get_dom d dom cl with
        | None -> raise Impossible
        | Some b ->
          let pexp = Delayed.mk_pexp d expprop (ExpNot(x)) in
          set_bool d pexp ncl (not b)
      end;
    | _ -> raise UnwaitedEvent

  let init d clsem cl =
    let v = ThE.sem clsem in
    let own = ThE.cl clsem in
    match is d own with
    | Some b ->
      let pexp = Delayed.mk_pexp d expprop (ExpNot((v,own,cl))) in
      set_bool d pexp cl (not b)
    | None ->
      match is d cl with
      | Some b ->
        let pexp = Delayed.mk_pexp d expprop
            (ExpNot((v,cl,own))) in
        set_bool d pexp own (not b)
      | None ->
        let events = [Demon.Create.EventDom(own,dom,(v,own,cl));
                      Demon.Create.EventDom(cl,dom,(v,cl,own))] in
        Demon.Fast.attach d key events

end

module RDaemonPropaNot = Demon.Fast.Register(DaemonPropaNot)

module DaemonPropa = struct
  type d =
  | Lit of ThE.t (* prop *) * int  (* watched *) * int (* otherside *)
  | All of ThE.t

  let key = Demon.Fast.create "Bool.DaemonPropa"

  module Data = struct
    type t = d
    let pp fmt = function
      | Lit (clsem,w,n) ->
        Format.fprintf fmt "Lit(%a,%i,%i,%a)" ThE.pp clsem w n
          Cl.pp (ThE.cl clsem)
      | All clsem -> Format.fprintf fmt "All(%a)" ThE.pp clsem
  end

  let immediate = false
  let throttle = 100

  let wakeup_lit ~first d clsem watched next =
    let v = ThE.sem clsem in
    let own = ThE.cl clsem in
    let pexp exp = Delayed.mk_pexp d expprop exp in
    let set_dom_up_true d own leaf _ =
      let b = (not v.topnot) in
      match Delayed.get_dom d dom own with
      | Some b' when b' == b -> ()
      | _ -> set_bool d (pexp (ExpUp(clsem,leaf))) own b in
    let merge_bcp cl sign =
      Debug.dprintf2 debug "[Bool] @[merge_bcp %a@]" Cl.pp cl;
      match Delayed.get_dom d dom own with
      | Some b' ->
        let b = mulbool sign (mulbool b' v.topnot) in
        let pexp = pexp (ExpBCP(clsem,cl,BCPOwnKnown)) in
        set_bool d pexp cl b
      | None -> (** merge *)
        match Delayed.get_dom d dom cl with
        | Some b' ->
          let b = mulbool sign (mulbool b' v.topnot) in
          let pexp = pexp (ExpBCP(clsem,cl,BCPLeavesKnown)) in
          set_bool d pexp own b
        | None -> (** merge *)
          if mulbool v.topnot sign
          then DaemonPropaNot.init d clsem cl
          else Delayed.merge d (pexp (ExpBCP(clsem,cl,BCP))) own cl in
    let rec find_watch dir pos bound =
      assert (dir == 1 || dir == -1);
      if pos == bound
      then
        let cl,sign = IArray.get v.lits pos in
        (merge_bcp cl sign; raise Exit)
      else
        let cl,sign = IArray.get v.lits pos in
        match Delayed.get_dom d dom cl with
        | None -> cl,pos
        | Some b when mulbool b sign (** true absorbent of or *) ->
          set_dom_up_true d own cl b; raise Exit
        | Some _ (** false *) -> find_watch dir (dir+pos) bound
    in
    try
      assert (watched <> next);
      let dir = if watched < next then 1 else -1 in
      let clwatched, watched = find_watch dir watched next in
      let clnext   , next    = find_watch (-dir) next watched in
      let events = [Demon.Create.EventDom(clwatched,dom,
                                          Lit(clsem,watched,next))] in
      let events =
        if first then
          Demon.Create.EventDom(clnext,dom,
                                       Lit(clsem,next,watched))::events
        else events in
      Demon.Fast.attach d key events;
      true
    with Exit -> false

  let wakeup_own ~first d clsem =
    let v = ThE.sem clsem in
    let own = ThE.cl clsem in
    let pexp exp = Delayed.mk_pexp d expprop exp in
    begin match Delayed.get_dom d dom own with
    | None -> assert (first);
      Demon.Fast.attach d key
        [Demon.Create.EventDom(own, dom, All clsem)];
      true
    (** \/ c1 c2 = false ==> c1 = false /\ c2 = false *)
    | Some b when not (mulbool v.topnot b) ->
      let set_dom_down_false (cl,sign) =
        set_bool d (pexp (ExpDown(clsem,cl))) cl sign in
      IArray.iter set_dom_down_false v.lits;
      false
    | Some _ -> true
    end

  (** return true if things should be propagated *)
  let init d clsem =
    let v = ThE.sem clsem in
    wakeup_own ~first:true d clsem &&
      let last = IArray.length v.lits - 1 in
      wakeup_lit ~first:true d clsem 0 last

  let wakeup d = function
    | Events.Fired.EventDom(_,dom',Lit(clsem,watched,next)) ->
      assert( Dom.equal dom dom' );
      ignore (wakeup_lit ~first:false d clsem watched next)
    | Events.Fired.EventDom(_ownr,dom',All clsem) ->
      assert( Dom.equal dom dom' );
      (** use this own because the other is the representant *)
      ignore (wakeup_own ~first:false d clsem)
    | _ -> raise UnwaitedEvent


end

module RDaemonPropa = Demon.Fast.Register(DaemonPropa)

module DaemonInit = struct
  let key = Demon.Fast.create "Bool.DaemonInit"

  module Data = DUnit

  let immediate = false
  let throttle = 100
  let wakeup d = function
    | Events.Fired.EventRegSem(clsem,()) ->
      begin try
          let clsem = ThE.coerce_clsem clsem in
          let v = ThE.sem clsem in
          match isnot v with
          | Some cl ->
            Delayed.register d cl;
            DaemonPropaNot.init d clsem cl
          | None ->
            assert (not lazy_propagation);
            IArray.iter (fun (cl,_) -> Delayed.register d cl) v.lits;
            if DaemonPropa.init d clsem then ()
        (* Delayed.ask_decision d (dec v) *)
        with Exit -> ()
      end
    | _ -> raise UnwaitedEvent

end

module RDaemonInit = Demon.Fast.Register(DaemonInit)

let th_register' with_other_theories env =
  with_other := with_other_theories;
  RDaemonPropaNot.init env;
  RDaemonPropa.init env;
  RDaemonInit.init env;
  Demon.Fast.attach env
    DaemonInit.key [Demon.Create.EventRegSem(sem,())];
  Delayed.register env cl_true;
  Delayed.register env cl_false;
  let pexp = Explanation.pexpfact in
  Delayed.set_dom env pexp dom cl_true true;
  Delayed.set_dom env pexp dom cl_false false;
  ()

let th_register_alone = th_register' false
let th_register = th_register' true

let _true = cl_true
let _not cl =
  index sem {topnot = true; lits = IArray.of_list [cl,false]}


let filter fold_left =
  let m = fold_left (fun acc (e,b) ->
      Cl.M.add_change (fun b -> b)
        (fun b1 b2 -> if b1 == b2 then b1 else raise Exit) e b acc)
      Cl.M.empty  in
  Cl.M.bindings m

let gen topnot l =
  try
    let l = filter (fun f acc -> List.fold_left f acc l) in
    match l with
    | [] -> if topnot then cl_true else cl_false
    | [cl,b] when mulbool topnot b -> _not cl
    | [cl,_] -> cl
    | l ->
      index sem {topnot; lits = IArray.of_list l}
  with Exit -> if topnot then cl_false else cl_true

let _or_and b l =
  try
    let l = filter (fun f acc ->
        List.fold_left (fun acc e -> f acc (e,b)) acc l) in
    match l with
    | [] -> if b then cl_true else cl_false
    | [a,b'] -> assert (b == b'); a
    | l ->
      index sem {topnot = b; lits = IArray.of_list l}
  with Exit -> if b then cl_false else cl_true

let _or  = _or_and false
let _and = _or_and true

let mk_clause m =
  if Cl.M.is_empty m then cl_false
  else let len = Cl.M.cardinal m in
    if len = 1 then
      let cl,b = Cl.M.choose m in
      if b then _not cl else cl
    else
      index sem {topnot=false;
                     lits = IArray.of_iter len
                         (fun iter -> Cl.M.iter (fun cl b -> iter (cl,b)) m)}

let _false = cl_false

let set_true env pexp cl = set_bool env pexp cl true

let set_false env pexp cl = set_bool env pexp cl false

let chobool = Explanation.Cho.create_key "Bool.cho"

let make_dec cl = Explanation.GCho(chobool,cl)

let () = Variable.register_sort ~dec:make_dec ty
(*
module ChoBool = struct
  open Conflict

  module Key = Cl
  module Data = DBool

  let make_decision env dec cl b =
    Debug.dprintf5 Conflict.print_decision
      "[Bool] decide %b on %a at %a" b Cl.pp cl Explanation.print_dec dec;
    let pexp = Explanation.mk_pcho dec chobool cl b in
    set_bool env pexp cl b

  let choose_decision env cl =
    match Solver.Delayed.get_dom env dom cl with
    | Some _ -> DecNo
    | None -> DecTodo true (** why not true? *)

  let analyse (type a) t (con: a Explanation.con) cl b =
    let return (s:bool Types.Cl.M.t) : a rescon =
      match Explanation.Con.Eq.eq_type conclause con with
      | None -> GOther (conclause,s)
      | Some Types.Eq -> GRequested s in
    ComputeConflict.set_dec_cho t chobool cl;
    return (Cl.M.singleton cl b)


  let key = chobool

end

module EChoBool = Conflict.RegisterCho(ChoBool)

let choclause = Explanation.Cho.create_key "Bool.choclause"

module ChoClause = struct
  module Key = Th
  module Data = struct
    type t = Cl.t * bool
    let pp fmt (cl,b) =
      Format.fprintf fmt "%a=%b" Cl.pp  cl b
  end

  let choose_decision env c =
    try
      Th.find_not_known env c.lits;
      Conflict.DecNo
    with Th.Found (cl,sign) ->
      Conflict.DecTodo (cl,not sign)

  let make_decision env dec _ (cl,b) =
    ChoBool.make_decision env dec cl b

  let analyse _ _ _ _ = assert false

  let key = choclause

end

module EChoClause = Conflict.RegisterCho(ChoClause)

open Conflict
type clause_conflict = bool Types.Cl.M.t

let mk_conequal:
  (ComputeConflict.t -> Cl.t -> Cl.t -> clause_conflict rescon) ref =
  ref (fun _ _ -> assert false)

let concat s1 s2 =
  Cl.M.union (fun _ b1 b2 -> assert (DBool.equal b1 b2); Some b1) s1 s2

let get_con acc t rescon =
  Conflict.fold_requested (fun acc _ s -> concat acc s)
    acc t rescon

let get_dom t age cl s =
  if Cl.equal cl cl_true || Cl.equal cl cl_false then s
  else
    let l = ComputeConflict.get_dom t age cl dom in
    (* Format.fprintf (Debug.get_debug_formatter ()) *)
    (*   "[get_dom] @[%a@]" *)
    (*   (Pp.list Pp.semi Explanation.print_mod_dom) l; *)
    List.fold_left (fun s mod_dom ->
        (* Format.fprintf (Debug.get_debug_formatter ()) *)
        (*   "[rlist] @[%a@]" *)
        (*   Conflict.print_rlist rlist; *)
        let pexp = mod_dom.Explanation.modpexp in
        let s = get_con s t (ComputeConflict.get_pexp t pexp conclause) in
        let s,deps =
          ComputeConflict.Equal.one_equal t
            ~from:cl
            ~to_:mod_dom.Explanation.modcl
            conclause
            s Explanation.Deps.empty
        in
        ComputeConflict.add_deps t deps;
        s) s l

let get_pexp t s pexp =
  get_con s t (ComputeConflict.get_pexp t pexp conclause)


let check_sem v cl =
  let own = ThE.cl (ThE.index v ty) in
  Cl.equal cl own

(** **)
module ExpMerge = struct
  open D

  type t = expmerge

  let pp fmt = function
    | Merge  (pexp,cl1,cl2)   ->
      Format.fprintf fmt "Merge!(%a,%a,%a)"
        Explanation.pp_pexp pexp Cl.pp cl1 Cl.pp cl2
    | DomMerge (pexp,cl) ->
      Format.fprintf fmt "DomMerge!(%a,%a)"
        Explanation.pp_pexp pexp Cl.pp cl

(*
  let need_dom t age cl =
    if not (Cl.equal cl cl_true || Cl.equal cl cl_false) then
      IterExp.need_dom t age cl dom

  let iterexp t age = function
    | Merge    (pexp,cl1,cl2,_)    ->
      IterExp.need_pexp t pexp;
      need_dom t age cl1;
      need_dom t age cl2
    | DomMerge (pexp,cl,_)    ->
      IterExp.need_pexp t pexp;
      need_dom t age cl
  *)

  let analyse :
      type a. Conflict.ComputeConflict.t ->
    Explanation.age -> a Explanation.con -> t -> a Conflict.rescon =
    fun t age con exp ->
    let return (s:bool Types.Cl.M.t) : a Conflict.rescon =
      match Explanation.Con.Eq.eq_type conclause con with
      | None -> GOther (conclause,s)
      | Some Types.Eq -> GRequested s in
    match exp with
    | Merge (pexp,cl1,cl2)    ->
      let s = Cl.M.empty in
      let s = get_con s t (ComputeConflict.get_pexp t pexp conclause) in
      let s = get_dom t age cl1 s in
      let s = get_dom t age cl2 s in
      return s
    | DomMerge (pexp,cl) ->
      let s = Cl.M.empty in
      let s = get_con s t (ComputeConflict.get_pexp t pexp conclause) in
      let s = get_dom t age cl s in
      return s

  let expdomlimit _ _ _ _ _ _ _ = raise Impossible (* used only for unsat *)

  let key = expmerge

  let same_sem t age _sem _v con exp _cl1 _cl2 =
    analyse  t age con exp

end

module EM = Conflict.RegisterExp(ExpMerge)

module ExpProp = struct

  type t = expprop

  let pp fmt = function
    | ExpBCP  (clsem,cl,kind) ->
      Format.fprintf fmt "Bcp(%a,%a = %a;%t)"
        ThE.pp clsem Cl.pp (ThE.cl clsem) Cl.pp cl
        (fun _ -> match kind with
           | BCPOwnKnown -> Format.fprintf fmt "Own"
           | BCPLeavesKnown -> Format.fprintf fmt "Leaves"
           | BCP -> ())
    | ExpUp  (clsem,leaf)    ->
      Format.fprintf fmt "Up(%a,%a <- %a)"
        ThE.pp clsem Cl.pp (ThE.cl clsem) Cl.pp leaf
    | ExpDown (clsem,cl)    ->
      Format.fprintf fmt "Down(%a,%a,%a ->)"
        ThE.pp clsem Cl.pp (ThE.cl clsem) Cl.pp cl
    | ExpNot ((v,clf,clt))    ->
      Format.fprintf fmt "Not(%a,%a,%a)"
        Th.pp v Cl.pp clf Cl.pp clt


(*
  let iterexp t age = function
    | ExpBCP  (v,_,_) when IArray.length v.lits = 1 ->
      raise Impossible
    | ExpBCP  (v,_,propa) ->
      IterExp.need_sem t age sem v;
      iter (fun (cl,_) ->
          if not (Cl.equal cl propa) then
          IterExp.need_dom t age cl dom) v
    | ExpUp  (v,_,leaf,_)    ->
      IterExp.need_sem t age sem v;
      IterExp.need_dom t age leaf dom
    | ExpDown (v,own)    ->
      IterExp.need_sem t age sem v;
      IterExp.need_dom t age own dom
    | ExpNot  (v,cl,_)->
      IterExp.need_sem t age sem v;
      IterExp.need_dom t age cl dom;
      (** For Top propagation (otherwise need_sem do already the job *)
      let cln,_ = IArray.get v.lits 0 in
      IterExp.need_cl_repr t age cln

*)

  let analyse :
      type a. Conflict.ComputeConflict.t ->
    Explanation.age -> a Explanation.con -> t -> a Conflict.rescon =
    fun t age con exp ->
    let s =
      match exp with
      | ExpBCP  (clsem,_,_) when IArray.length (ThE.sem clsem).lits = 1 ->
        raise Impossible
      | ExpBCP  (clsem,propa,kind) ->
        let v = ThE.sem clsem in
        let own = ThE.cl clsem in
        let s = Cl.M.empty in
        let s = if kind == BCPOwnKnown then get_dom t age own s else s in
        fold (fun s (cl,_) ->
          if kind != BCPLeavesKnown && (Cl.equal cl propa) then s
          else get_dom t age cl s) s v
      | ExpUp (_,leaf)    ->
        let s = Cl.M.empty in
        let s = get_dom  t age leaf s in
        s
      | ExpDown  (clsem,_)    ->
        let own = ThE.cl clsem in
        let s = Cl.M.empty in
        let s = get_dom  t age own s in
        s
      | ExpNot  ((v,clfrom,clto))->
        let s = Cl.M.empty in
        assert (check_sem v clto || check_sem v clfrom);
        let s = get_dom t age clfrom s in
        fold (fun s (cl,_) ->
          if (Cl.equal cl clfrom) ||
             (Cl.equal cl clto) then s
          else get_dom t age cl s) s v
    in
    Conflict.return con conclause s

  let expdomlimit :
    type a b. Conflict.ComputeConflict.t ->
      Explanation.age -> b dom -> Cl.t ->
      a Explanation.con -> b option -> t -> a Conflict.rescon =
    fun t age dom' cl con b _ ->
      (* should not fail since we only set the domain *)
      let b = Opt.get_exn Impossible b in
      let b = Dom.Eq.coerce dom' dom b in
      if ComputeConflict.before_first_dec t age
      then Conflict.return con conclause Cl.M.empty
      else Conflict.return con conclause (Cl.M.singleton cl b)

  let key = expprop

  let same_sem t age _sem _v con exp _cl1 _cl2 =
    analyse  t age con exp

end

module EP = Conflict.RegisterExp(ExpProp)

module ConClause = struct
  open Conflict

  type t = bool Cl.M.t

  let pp fmt t =
    Format.fprintf fmt "@[%a@]"
      (Pp.iter2 Cl.M.iter (Pp.constant_formatted "⋀@,")
         (Pp.constant_formatted "=") Cl.pp
         Format.pp_print_bool)
      t

  let key = conclause

  class finalized v : Conflict.finalized = object
    method pp fmt =
      Pp.iter2 Cl.M.iter Pp.semi Pp.comma Cl.pp DBool.pp fmt v
    method test d =
      try
        Cl.M.fold_left (fun acc cl sign ->
            match is d cl with
            | None -> ToDecide
            | Some b ->
              if mulbool b sign
              then raise Exit
              else acc) False v
      with Exit -> True
    method decide :
      'a. 'a Conflict.fold_decisions -> Solver.Delayed.t -> 'a -> 'a =
      fun f d acc ->
        Cl.M.fold_left (fun acc cl b ->
            match is d cl with
            | None ->
              f.fold_decisions acc chobool cl (not b)
            | Some _ -> acc) acc v
    method conflict_add _ = v
  end

  let finalize _ sl =
    let s = Bag.fold_left union_disjoint Cl.M.empty sl in
    Debug.dprintf2 Conflict.print_conflicts "[Bool] @[conflict: %a@]"
      pp s;
    match Cl.M.cardinal s with
    | 0 -> None
    | _ ->
      Some (new finalized s)

  let eq_sym s = s
  let eq_transitivity s1 s2 = concat s1 s2
  let eq_check ~from:_ ~to_:_ _ = true
  let eq_other ~from:_ ~to_:_ = Cl.M.empty
  let finish _ x = x

  let clatlimit t age clo rcl =
    if ComputeConflict.before_first_dec t age
    then GRequested Cl.M.empty
    else
      match clo, rcl with
      | (_true, cl) when Cl.equal _true cl_true ->
        ComputeConflict.set_dec_cho t chobool cl;
        GRequested (Cl.M.singleton cl true)
      | (cl, _true) when Cl.equal _true cl_true ->
        ComputeConflict.set_dec_cho t chobool cl;
        GRequested (Cl.M.singleton cl true)
      | (_false, cl) when Cl.equal _false cl_false ->
        ComputeConflict.set_dec_cho t chobool cl;
        GRequested (Cl.M.singleton cl false)
      | (cl, _false) when Cl.equal _false cl_false ->
        ComputeConflict.set_dec_cho t chobool cl;
        GRequested (Cl.M.singleton cl false)
      | _ ->
        !mk_conequal t clo rcl

  (* let propadom: *)
  (*   type a. ComputeConflict.t -> *)
  (*     Explanation.Age.t -> a dom -> Cl.t -> a option -> t rescon = *)
  (*     fun t age dom' cl bval -> *)
  (*       if ComputeConflict.before_first_dec t age *)
  (*       then GRequested Cl.M.empty *)
  (*       else *)
  (*         match Dom.Eq.coerce_type dom dom' with *)
  (*         | Types.Eq -> *)
  (*           if Cl.equal cl cl_true || Cl.equal cl cl_false then *)
  (*             GRequested Cl.M.empty *)
  (*           else *)
  (*             GRequested (Cl.M.singleton cl (Opt.get bval)) *)
end

module EC = Conflict.RegisterCon(ConClause)
*)
