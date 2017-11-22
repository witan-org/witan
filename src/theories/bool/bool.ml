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

open Stdlib
open Witan_core
open Solver

let lazy_propagation = false

let debug = Debug.register_info_flag
  ~desc:"for the boolean theory"
  "bool"

let ty_ctr = Ty.Constr.create "Bool"
let ty = Ty.ctr ty_ctr
let dom : bool Value.t = Value.create_key "bool"

module BoolValue = Value.Register(struct
    include DBool
    let key = dom
  end)

let value_true = BoolValue.index true ty
let cl_true = BoolValue.node value_true
let () = Node.rename cl_true "⊤"

let value_false = BoolValue.index false ty
let cl_false = BoolValue.node value_false
let () = Node.rename cl_false "⊤"

let union_disjoint m1 m2 =
  Node.M.union (fun _ b1 b2 -> assert (b1 == b2); Some b1) m1 m2

let index sem v = Node.index sem v ty

let with_other = ref false
(** TODO not a global variable...
    merge the element that are true instead of using only the dom.
    false by default
*)


let is env node = Solver.Delayed.get_value env dom node
let is_true  env node = Node.equal node cl_true || is env node = Some true
let is_false env node = Node.equal node cl_false || is env node = Some false
let is_unknown env node = is env node = None

(*
module D = struct
  type t = bool
  let merged b1 b2 = Opt.equal (==) b1 b2

  type expmerge =
  | Merge of Explanation.pexp * Node.t * Node.t
  | DomMerge of Explanation.pexp * Node.t

  let expmerge : expmerge Explanation.exp =
    Explanation.Exp.create_key "Bool.merge"

  let true_is_false d node pexp =
    let pexp = Delayed.mk_pexp d expmerge (DomMerge(pexp,node)) in
    Delayed.contradiction d pexp

  let set_bool env pexp node b =
    if !with_other then
      Delayed.merge env pexp node
        (if b then cl_true else cl_false)
    else
      match Delayed.get_value env dom node with
      | Some b' when DBool.equal b b' -> ()
      | Some _ ->
        true_is_false env node pexp
      | None ->
        Delayed.set_value env pexp dom node b

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
*)


let set_bool env pexp node b =
  if !with_other then
    Delayed.merge env pexp node
      (if b then cl_true else cl_false)
  else
    Delayed.set_value env pexp dom node b

type t =
  { topnot: bool;
    lits: (Node.t * bool) IArray.t;
  }

let sem : t Sem.t = Sem.create_key "Prop"

(* let iter f x = IArray.iter f x.lits *)

let fold f acc x = IArray.fold f acc x.lits

let isnot v =
  if IArray.length v.lits == 1 then
    let node,sign = IArray.get v.lits 0 in
    assert (v.topnot && not sign);
    Some node
  else
    None

let mulbool b1 b2 = b1 != b2

module T = struct
  type r = t
  type t = r
  let equal n1 n2 =
    let clbool (cl1,b1) (cl2,b2) = Node.equal cl1 cl2 && DBool.equal b1 b2 in
    n1.topnot == n2.topnot &&
    IArray.equal clbool n1.lits n2.lits

  let hash n =
    let clbool (node,b) = Hashcons.combine (Node.hash node) (DBool.hash b) in
    Hashcons.combine (DBool.hash n.topnot) (IArray.hash clbool n.lits)

  let compare n1 n2 =
    let c = DBool.compare n1.topnot n2.topnot in
    if c != 0 then c else
      let clbool (cl1,b1) (cl2,b2) =
        let c = Node.compare cl1 cl2 in
        if c != 0 then c
        else DBool.compare b1 b2 in
      IArray.compare clbool n1.lits n2.lits


  let print_cl fmt node b =
    if b
    then Format.fprintf fmt "¬ %a" Node.pp node
    else Node.pp fmt node

  let pp fmt x =
    match isnot x with
    | Some node ->
      print_cl fmt node true
    | None ->
      let print_cl topnot fmt (node,b) = print_cl fmt node (mulbool topnot b) in
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

  exception Found of Node.t * bool
  let find_not_known d l =
    IArray.iter (fun (node,b) ->
      match Delayed.get_value d dom node with
      | Some _ -> ()
      | None -> raise (Found (node,b))
    ) l

  let _bcp d l absorbent =
    try
      let res = IArray.fold (fun acc node ->
        match Delayed.get_value d dom node, acc with
        | Some b, _ when b = absorbent -> raise (TopKnown absorbent)
        | Some _, _ -> acc
        | None, Some _ -> raise Exit
        | None, None -> Some node)
        None l in
      match res with
      | None -> raise (TopKnown (not absorbent))
      | _ -> res
    with Exit -> None

end

module ThE = Sem.Register(Th)

type bcpkind =
  | BCPOwnKnown
  | BCPLeavesKnown
  | BCP

type expprop =
| ExpBCP  of ThE.t (* own *) * Node.t (* propa *) * bcpkind
| ExpUp  of ThE.t (* own *) * Node.t  (* one leaf to own *)
| ExpDown of ThE.t (* own *) * Node.t (* leaf *)(* own to leaf *)
| ExpNot  of (Th.t * Node.t * Node.t)
(* | ExpNot  of Th.t * Node.t * Node.t (* t <> not t or t = not (not t) *) *)

let expprop : expprop Explanation.exp =
  Explanation.Exp.create_key "Bool.prop"

module DaemonPropaNot = struct

  module Data = struct
    type t = Th.t * Node.t * Node.t
    let pp fmt (v,cl1,cl2) =
      Format.fprintf fmt "%a,%a -> %a" Th.pp v Node.pp cl1 Node.pp cl2
  end

  let immediate = false
  let key = Demon.Fast.create "Bool.DaemonPropaNot"
  let throttle = 100
  let wakeup d =
    function
    | Events.Fired.EventValue(_,dom',((_,node,ncl) as x)) ->
      assert (Value.equal dom dom');
      begin match Delayed.get_value d dom node with
        | None -> raise Impossible
        | Some b ->
          let pexp = Delayed.mk_pexp d expprop (ExpNot(x)) in
          set_bool d pexp ncl (not b)
      end;
    | _ -> raise UnwaitedEvent

  let init d nodesem node =
    let v = ThE.sem nodesem in
    let own = ThE.node nodesem in
    match is d own with
    | Some b ->
      let pexp = Delayed.mk_pexp d expprop (ExpNot((v,own,node))) in
      set_bool d pexp node (not b)
    | None ->
      match is d node with
      | Some b ->
        let pexp = Delayed.mk_pexp d expprop
            (ExpNot((v,node,own))) in
        set_bool d pexp own (not b)
      | None ->
        let events = [Demon.Create.EventValue(own,dom,(v,own,node));
                      Demon.Create.EventValue(node,dom,(v,node,own))] in
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
      | Lit (nodesem,w,n) ->
        Format.fprintf fmt "Lit(%a,%i,%i,%a)" ThE.pp nodesem w n
          Node.pp (ThE.node nodesem)
      | All nodesem -> Format.fprintf fmt "All(%a)" ThE.pp nodesem
  end

  let immediate = false
  let throttle = 100

  let wakeup_lit ~first d nodesem watched next =
    let v = ThE.sem nodesem in
    let own = ThE.node nodesem in
    let pexp exp = Delayed.mk_pexp d expprop exp in
    let set_dom_up_true d own leaf _ =
      let b = (not v.topnot) in
      match Delayed.get_value d dom own with
      | Some b' when b' == b -> ()
      | _ -> set_bool d (pexp (ExpUp(nodesem,leaf))) own b in
    let merge_bcp node sign =
      Debug.dprintf2 debug "[Bool] @[merge_bcp %a@]" Node.pp node;
      match Delayed.get_value d dom own with
      | Some b' ->
        let b = mulbool sign (mulbool b' v.topnot) in
        let pexp = pexp (ExpBCP(nodesem,node,BCPOwnKnown)) in
        set_bool d pexp node b
      | None -> (** merge *)
        match Delayed.get_value d dom node with
        | Some b' ->
          let b = mulbool sign (mulbool b' v.topnot) in
          let pexp = pexp (ExpBCP(nodesem,node,BCPLeavesKnown)) in
          set_bool d pexp own b
        | None -> (** merge *)
          if mulbool v.topnot sign
          then DaemonPropaNot.init d nodesem node
          else Delayed.merge d (pexp (ExpBCP(nodesem,node,BCP))) own node in
    let rec find_watch dir pos bound =
      assert (dir == 1 || dir == -1);
      if pos == bound
      then
        let node,sign = IArray.get v.lits pos in
        (merge_bcp node sign; raise Exit)
      else
        let node,sign = IArray.get v.lits pos in
        match Delayed.get_value d dom node with
        | None -> node,pos
        | Some b when mulbool b sign (** true absorbent of or *) ->
          set_dom_up_true d own node b; raise Exit
        | Some _ (** false *) -> find_watch dir (dir+pos) bound
    in
    try
      assert (watched <> next);
      let dir = if watched < next then 1 else -1 in
      let clwatched, watched = find_watch dir watched next in
      let clnext   , next    = find_watch (-dir) next watched in
      let events = [Demon.Create.EventValue(clwatched,dom,
                                          Lit(nodesem,watched,next))] in
      let events =
        if first then
          Demon.Create.EventValue(clnext,dom,
                                       Lit(nodesem,next,watched))::events
        else events in
      Demon.Fast.attach d key events;
      true
    with Exit -> false

  let wakeup_own ~first d nodesem =
    let v = ThE.sem nodesem in
    let own = ThE.node nodesem in
    let pexp exp = Delayed.mk_pexp d expprop exp in
    begin match Delayed.get_value d dom own with
    | None -> assert (first);
      Demon.Fast.attach d key
        [Demon.Create.EventValue(own, dom, All nodesem)];
      true
    (** \/ c1 c2 = false ==> c1 = false /\ c2 = false *)
    | Some b when not (mulbool v.topnot b) ->
      let set_dom_down_false (node,sign) =
        set_bool d (pexp (ExpDown(nodesem,node))) node sign in
      IArray.iter set_dom_down_false v.lits;
      false
    | Some _ -> true
    end

  (** return true if things should be propagated *)
  let init d nodesem =
    let v = ThE.sem nodesem in
    wakeup_own ~first:true d nodesem &&
      let last = IArray.length v.lits - 1 in
      wakeup_lit ~first:true d nodesem 0 last

  let wakeup d = function
    | Events.Fired.EventValue(_,dom',Lit(nodesem,watched,next)) ->
      assert( Value.equal dom dom' );
      ignore (wakeup_lit ~first:false d nodesem watched next)
    | Events.Fired.EventValue(_ownr,dom',All nodesem) ->
      assert( Value.equal dom dom' );
      (** use this own because the other is the representant *)
      ignore (wakeup_own ~first:false d nodesem)
    | _ -> raise UnwaitedEvent


end

module RDaemonPropa = Demon.Fast.Register(DaemonPropa)

module DaemonInit = struct
  let key = Demon.Fast.create "Bool.DaemonInit"

  module Data = DUnit

  let immediate = false
  let throttle = 100
  let wakeup d = function
    | Events.Fired.EventRegSem(nodesem,()) ->
      begin try
          let nodesem = ThE.coerce_clsem nodesem in
          let v = ThE.sem nodesem in
          match isnot v with
          | Some node ->
            Delayed.register d node;
            DaemonPropaNot.init d nodesem node
          | None ->
            assert (not lazy_propagation);
            IArray.iter (fun (node,_) -> Delayed.register d node) v.lits;
            if DaemonPropa.init d nodesem then ()
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
  ()

let th_register_alone = th_register' false
let th_register = th_register' true

let _true = cl_true
let _not node =
  index sem {topnot = true; lits = IArray.of_list [node,false]}


let filter fold_left =
  let m = fold_left (fun acc (e,b) ->
      Node.M.add_change (fun b -> b)
        (fun b1 b2 -> if b1 == b2 then b1 else raise Exit) e b acc)
      Node.M.empty  in
  Node.M.bindings m

let gen topnot l =
  try
    let l = filter (fun f acc -> List.fold_left f acc l) in
    match l with
    | [] -> if topnot then cl_true else cl_false
    | [node,b] when mulbool topnot b -> _not node
    | [node,_] -> node
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
  if Node.M.is_empty m then cl_false
  else let len = Node.M.cardinal m in
    if len = 1 then
      let node,b = Node.M.choose m in
      if b then _not node else node
    else
      index sem {topnot=false;
                     lits = IArray.of_iter len
                         (fun iter -> Node.M.iter (fun node b -> iter (node,b)) m)}

let _false = cl_false

let set_true env pexp node = set_bool env pexp node true

let set_false env pexp node = set_bool env pexp node false

let chobool = Explanation.Cho.create_key "Bool.cho"

let make_dec node = Explanation.GCho(chobool,node)

let () = Variable.register_sort ~dec:make_dec ty
(*
module ChoBool = struct
  open Conflict

  module Key = Node
  module Data = DBool

  let make_decision env dec node b =
    Debug.dprintf5 Conflict.print_decision
      "[Bool] decide %b on %a at %a" b Node.pp node Explanation.print_dec dec;
    let pexp = Explanation.mk_pcho dec chobool node b in
    set_bool env pexp node b

  let choose_decision env node =
    match Solver.Delayed.get_dom env dom node with
    | Some _ -> DecNo
    | None -> DecTodo true (** why not true? *)

  let analyse (type a) t (con: a Explanation.con) node b =
    let return (s:bool Types.Node.M.t) : a rescon =
      match Explanation.Con.Eq.eq_type conclause con with
      | None -> GOther (conclause,s)
      | Some Types.Eq -> GRequested s in
    ComputeConflict.set_dec_cho t chobool node;
    return (Node.M.singleton node b)


  let key = chobool

end

module EChoBool = Conflict.RegisterCho(ChoBool)

let choclause = Explanation.Cho.create_key "Bool.choclause"

module ChoClause = struct
  module Key = Th
  module Data = struct
    type t = Node.t * bool
    let pp fmt (node,b) =
      Format.fprintf fmt "%a=%b" Node.pp  node b
  end

  let choose_decision env c =
    try
      Th.find_not_known env c.lits;
      Conflict.DecNo
    with Th.Found (node,sign) ->
      Conflict.DecTodo (node,not sign)

  let make_decision env dec _ (node,b) =
    ChoBool.make_decision env dec node b

  let analyse _ _ _ _ = assert false

  let key = choclause

end

module EChoClause = Conflict.RegisterCho(ChoClause)

open Conflict
type clause_conflict = bool Types.Node.M.t

let mk_conequal:
  (ComputeConflict.t -> Node.t -> Node.t -> clause_conflict rescon) ref =
  ref (fun _ _ -> assert false)

let concat s1 s2 =
  Node.M.union (fun _ b1 b2 -> assert (DBool.equal b1 b2); Some b1) s1 s2

let get_con acc t rescon =
  Conflict.fold_requested (fun acc _ s -> concat acc s)
    acc t rescon

let get_dom t age node s =
  if Node.equal node cl_true || Node.equal node cl_false then s
  else
    let l = ComputeConflict.get_dom t age node dom in
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
            ~from:node
            ~to_:mod_dom.Explanation.modcl
            conclause
            s Explanation.Deps.empty
        in
        ComputeConflict.add_deps t deps;
        s) s l

let get_pexp t s pexp =
  get_con s t (ComputeConflict.get_pexp t pexp conclause)


let check_sem v node =
  let own = ThE.node (ThE.index v ty) in
  Node.equal node own

(** **)
module ExpMerge = struct
  open D

  type t = expmerge

  let pp fmt = function
    | Merge  (pexp,cl1,cl2)   ->
      Format.fprintf fmt "Merge!(%a,%a,%a)"
        Explanation.pp_pexp pexp Node.pp cl1 Node.pp cl2
    | DomMerge (pexp,node) ->
      Format.fprintf fmt "DomMerge!(%a,%a)"
        Explanation.pp_pexp pexp Node.pp node

(*
  let need_dom t age node =
    if not (Node.equal node cl_true || Node.equal node cl_false) then
      IterExp.need_dom t age node dom

  let iterexp t age = function
    | Merge    (pexp,cl1,cl2,_)    ->
      IterExp.need_pexp t pexp;
      need_dom t age cl1;
      need_dom t age cl2
    | DomMerge (pexp,node,_)    ->
      IterExp.need_pexp t pexp;
      need_dom t age node
  *)

  let analyse :
      type a. Conflict.ComputeConflict.t ->
    Explanation.age -> a Explanation.con -> t -> a Conflict.rescon =
    fun t age con exp ->
    let return (s:bool Types.Node.M.t) : a Conflict.rescon =
      match Explanation.Con.Eq.eq_type conclause con with
      | None -> GOther (conclause,s)
      | Some Types.Eq -> GRequested s in
    match exp with
    | Merge (pexp,cl1,cl2)    ->
      let s = Node.M.empty in
      let s = get_con s t (ComputeConflict.get_pexp t pexp conclause) in
      let s = get_dom t age cl1 s in
      let s = get_dom t age cl2 s in
      return s
    | DomMerge (pexp,node) ->
      let s = Node.M.empty in
      let s = get_con s t (ComputeConflict.get_pexp t pexp conclause) in
      let s = get_dom t age node s in
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
    | ExpBCP  (nodesem,node,kind) ->
      Format.fprintf fmt "Bcp(%a,%a = %a;%t)"
        ThE.pp nodesem Node.pp (ThE.node nodesem) Node.pp node
        (fun _ -> match kind with
           | BCPOwnKnown -> Format.fprintf fmt "Own"
           | BCPLeavesKnown -> Format.fprintf fmt "Leaves"
           | BCP -> ())
    | ExpUp  (nodesem,leaf)    ->
      Format.fprintf fmt "Up(%a,%a <- %a)"
        ThE.pp nodesem Node.pp (ThE.node nodesem) Node.pp leaf
    | ExpDown (nodesem,node)    ->
      Format.fprintf fmt "Down(%a,%a,%a ->)"
        ThE.pp nodesem Node.pp (ThE.node nodesem) Node.pp node
    | ExpNot ((v,clf,clt))    ->
      Format.fprintf fmt "Not(%a,%a,%a)"
        Th.pp v Node.pp clf Node.pp clt


(*
  let iterexp t age = function
    | ExpBCP  (v,_,_) when IArray.length v.lits = 1 ->
      raise Impossible
    | ExpBCP  (v,_,propa) ->
      IterExp.need_sem t age sem v;
      iter (fun (node,_) ->
          if not (Node.equal node propa) then
          IterExp.need_dom t age node dom) v
    | ExpUp  (v,_,leaf,_)    ->
      IterExp.need_sem t age sem v;
      IterExp.need_dom t age leaf dom
    | ExpDown (v,own)    ->
      IterExp.need_sem t age sem v;
      IterExp.need_dom t age own dom
    | ExpNot  (v,node,_)->
      IterExp.need_sem t age sem v;
      IterExp.need_dom t age node dom;
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
      | ExpBCP  (nodesem,_,_) when IArray.length (ThE.sem nodesem).lits = 1 ->
        raise Impossible
      | ExpBCP  (nodesem,propa,kind) ->
        let v = ThE.sem nodesem in
        let own = ThE.node nodesem in
        let s = Node.M.empty in
        let s = if kind == BCPOwnKnown then get_dom t age own s else s in
        fold (fun s (node,_) ->
          if kind != BCPLeavesKnown && (Node.equal node propa) then s
          else get_dom t age node s) s v
      | ExpUp (_,leaf)    ->
        let s = Node.M.empty in
        let s = get_dom  t age leaf s in
        s
      | ExpDown  (nodesem,_)    ->
        let own = ThE.node nodesem in
        let s = Node.M.empty in
        let s = get_dom  t age own s in
        s
      | ExpNot  ((v,clfrom,clto))->
        let s = Node.M.empty in
        assert (check_sem v clto || check_sem v clfrom);
        let s = get_dom t age clfrom s in
        fold (fun s (node,_) ->
          if (Node.equal node clfrom) ||
             (Node.equal node clto) then s
          else get_dom t age node s) s v
    in
    Conflict.return con conclause s

  let expdomlimit :
    type a b. Conflict.ComputeConflict.t ->
      Explanation.age -> b dom -> Node.t ->
      a Explanation.con -> b option -> t -> a Conflict.rescon =
    fun t age dom' node con b _ ->
      (* should not fail since we only set the domain *)
      let b = Opt.get_exn Impossible b in
      let b = Dom.Eq.coerce dom' dom b in
      if ComputeConflict.before_first_dec t age
      then Conflict.return con conclause Node.M.empty
      else Conflict.return con conclause (Node.M.singleton node b)

  let key = expprop

  let same_sem t age _sem _v con exp _cl1 _cl2 =
    analyse  t age con exp

end

module EP = Conflict.RegisterExp(ExpProp)

module ConClause = struct
  open Conflict

  type t = bool Node.M.t

  let pp fmt t =
    Format.fprintf fmt "@[%a@]"
      (Pp.iter2 Node.M.iter (Pp.constant_formatted "⋀@,")
         (Pp.constant_formatted "=") Node.pp
         Format.pp_print_bool)
      t

  let key = conclause

  class finalized v : Conflict.finalized = object
    method pp fmt =
      Pp.iter2 Node.M.iter Pp.semi Pp.comma Node.pp DBool.pp fmt v
    method test d =
      try
        Node.M.fold_left (fun acc node sign ->
            match is d node with
            | None -> ToDecide
            | Some b ->
              if mulbool b sign
              then raise Exit
              else acc) False v
      with Exit -> True
    method decide :
      'a. 'a Conflict.fold_decisions -> Solver.Delayed.t -> 'a -> 'a =
      fun f d acc ->
        Node.M.fold_left (fun acc node b ->
            match is d node with
            | None ->
              f.fold_decisions acc chobool node (not b)
            | Some _ -> acc) acc v
    method conflict_add _ = v
  end

  let finalize _ sl =
    let s = Bag.fold_left union_disjoint Node.M.empty sl in
    Debug.dprintf2 Conflict.print_conflicts "[Bool] @[conflict: %a@]"
      pp s;
    match Node.M.cardinal s with
    | 0 -> None
    | _ ->
      Some (new finalized s)

  let eq_sym s = s
  let eq_transitivity s1 s2 = concat s1 s2
  let eq_check ~from:_ ~to_:_ _ = true
  let eq_other ~from:_ ~to_:_ = Node.M.empty
  let finish _ x = x

  let clatlimit t age clo rcl =
    if ComputeConflict.before_first_dec t age
    then GRequested Node.M.empty
    else
      match clo, rcl with
      | (_true, node) when Node.equal _true cl_true ->
        ComputeConflict.set_dec_cho t chobool node;
        GRequested (Node.M.singleton node true)
      | (node, _true) when Node.equal _true cl_true ->
        ComputeConflict.set_dec_cho t chobool node;
        GRequested (Node.M.singleton node true)
      | (_false, node) when Node.equal _false cl_false ->
        ComputeConflict.set_dec_cho t chobool node;
        GRequested (Node.M.singleton node false)
      | (node, _false) when Node.equal _false cl_false ->
        ComputeConflict.set_dec_cho t chobool node;
        GRequested (Node.M.singleton node false)
      | _ ->
        !mk_conequal t clo rcl

  (* let propadom: *)
  (*   type a. ComputeConflict.t -> *)
  (*     Explanation.Age.t -> a dom -> Node.t -> a option -> t rescon = *)
  (*     fun t age dom' node bval -> *)
  (*       if ComputeConflict.before_first_dec t age *)
  (*       then GRequested Node.M.empty *)
  (*       else *)
  (*         match Dom.Eq.coerce_type dom dom' with *)
  (*         | Types.Eq -> *)
  (*           if Node.equal node cl_true || Node.equal node cl_false then *)
  (*             GRequested Node.M.empty *)
  (*           else *)
  (*             GRequested (Node.M.singleton node (Opt.get bval)) *)
end

module EC = Conflict.RegisterCon(ConClause)
*)
