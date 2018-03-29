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
open Std
open Witan_core
open Egraph

let lazy_propagation = false

let debug = Debug.register_info_flag
  ~desc:"for the boolean theory"
  "bool"

let ty = Term._Prop
let dom : bool Value.t = Value.create_key "bool"

module BoolValue = Value.Register(struct
    include DBool
    let key = dom
  end)

let value_true = BoolValue.index ~basename:"⊤" true ty
let values_true = BoolValue.nodevalue value_true
let node_true = BoolValue.node value_true

let value_false = BoolValue.index ~basename:"⊥" false ty
let values_false = BoolValue.nodevalue value_false
let node_false = BoolValue.node value_false

let union_disjoint m1 m2 =
  Node.M.union (fun _ b1 b2 -> assert (b1 == b2); Some b1) m1 m2

let index sem v = Node.index_sem sem v ty

let with_other = ref false
(** TODO not a global variable...
    merge the element that are true instead of using only the dom.
    false by default
*)


let is env node = Egraph.Delayed.get_value env dom node
let is_true  env node = Node.equal node node_true || is env node = Some true
let is_false env node = Node.equal node node_false || is env node = Some false
let is_unknown env node = is env node = None

(*
module D = struct
  type t = bool
  let merged b1 b2 = Opt.equal (==) b1 b2

  type expmerge =
  | Merge of Trail.pexp * Node.t * Node.t
  | DomMerge of Trail.pexp * Node.t

  let expmerge : expmerge Trail.exp =
    Trail.Exp.create_key "Bool.merge"

  let true_is_false d node pexp =
    let pexp = Delayed.mk_pexp d expmerge (DomMerge(pexp,node)) in
    Delayed.contradiction d pexp

  let set_bool env pexp node b =
    if !with_other then
      Delayed.merge env pexp node
        (if b then node_true else node_false)
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
      (if b then node_true else node_false)
  else
    Delayed.set_value env pexp dom node b

type t =
  { topnot: bool;
    lits: (Node.t * bool) IArray.t;
  }

let sem : t Sem.t = Sem.create_key "Prop"

(* let iter f x = IArray.iter f x.lits *)

let fold f acc x = IArray.fold f acc x.lits

let find x n =
  fold (fun acc (n1,b) -> if Node.equal n1 n then Some b else acc) None x

let isnot v =
  if IArray.length v.lits == 1 then
    let node,sign = IArray.get v.lits 0 in
    assert (v.topnot && not sign);
    Some node
  else
    None

let mulbool b1 b2 = b1 != b2

let node_of_bool b =
  if b then node_true else node_false

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

(** At least all the leaves except one are known and can be discarded *)
type bcpkind =
  | BCPOwnKnown      (** Top is known and true modulo topnot, propagate true modulo sign to propa *)
  | BCPLeavesKnown   (** All leaves are known and false modulo sign, propagate false modulo topnot to own *)
  | BCP              (** Merge top with the remaining leave *)

type expprop =
| ExpBCP  of ThE.t (* own *) * Node.t (* propa *) * bcpkind
| ExpUp  of ThE.t (* own *) * Node.t  (* one leaf to own *)
| ExpDown of ThE.t (* own *) * Node.t (* leaf *)(* own to leaf *)
| ExpNot  of (Th.t * Node.t * Node.t) * bool (* what have been propagated *)
| ExpDec  of Node.t * bool

let expprop : expprop Exp.t = Exp.create_key "Bool.prop"

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
          let pexp = Delayed.mk_pexp d expprop (ExpNot(x,not b)) in
          set_bool d pexp ncl (not b)
      end;
    | _ -> raise UnwaitedEvent

  let init d thterm node =
    let v = ThE.sem thterm in
    let own = ThE.node thterm in
    match is d own with
    | Some b ->
      let pexp = Delayed.mk_pexp d expprop (ExpNot((v,own,node),not b)) in
      set_bool d pexp node (not b)
    | None ->
      match is d node with
      | Some b ->
        let pexp = Delayed.mk_pexp d expprop
            (ExpNot((v,node,own),not b)) in
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
      | Lit (thterm,w,n) ->
        Format.fprintf fmt "Lit(%a,%i,%i,%a)" ThE.pp thterm w n
          Node.pp (ThE.node thterm)
      | All thterm -> Format.fprintf fmt "All(%a)" ThE.pp thterm
  end

  let immediate = false
  let throttle = 100

  let wakeup_lit ~first d thterm watched next =
    let v = ThE.sem thterm in
    let own = ThE.node thterm in
    let pexp exp = Delayed.mk_pexp d expprop exp in
    let set_dom_up_true d own leaf _ =
      let b = (not v.topnot) in
      match Delayed.get_value d dom own with
      | Some b' when b' == b -> ()
      | _ -> set_bool d (pexp (ExpUp(thterm,leaf))) own b in
    let merge_bcp node sign =
      Debug.dprintf2 debug "[Bool] @[merge_bcp %a@]" Node.pp node;
      match Delayed.get_value d dom own with
      | Some b' ->
        let pexp = if (mulbool b' v.topnot)
          then pexp (ExpBCP(thterm,node,BCPOwnKnown))
          else pexp (ExpDown(thterm,node))
        in
        let b = mulbool sign (mulbool b' v.topnot) in
        set_bool d pexp node b
      | None -> (** merge *)
        match Delayed.get_value d dom node with
        | Some b' ->
          let pexp = if (mulbool b' sign)
            then pexp (ExpUp(thterm,node))
            else pexp (ExpBCP(thterm,node,BCPLeavesKnown))
          in
          let b = mulbool sign (mulbool b' v.topnot) in
          set_bool d pexp own b
        | None -> (** merge *)
          if mulbool v.topnot sign
          then DaemonPropaNot.init d thterm node
          else Delayed.merge d (pexp (ExpBCP(thterm,node,BCP))) own node in
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
                                          Lit(thterm,watched,next))] in
      let events =
        if first then
          Demon.Create.EventValue(clnext,dom,
                                       Lit(thterm,next,watched))::events
        else events in
      Demon.Fast.attach d key events;
      true
    with Exit -> false

  let wakeup_own ~first d thterm =
    let v = ThE.sem thterm in
    let own = ThE.node thterm in
    let pexp exp = Delayed.mk_pexp d expprop exp in
    begin match Delayed.get_value d dom own with
    | None -> assert (first);
      Demon.Fast.attach d key
        [Demon.Create.EventValue(own, dom, All thterm)];
      true
    (** \/ c1 c2 = false ==> c1 = false /\ c2 = false *)
    | Some b when not (mulbool v.topnot b) ->
      let set_dom_down_false (node,sign) =
        set_bool d (pexp (ExpDown(thterm,node))) node sign in
      IArray.iter set_dom_down_false v.lits;
      false
    | Some _ -> true
    end

  (** return true if things should be propagated *)
  let init d thterm =
    let v = ThE.sem thterm in
    wakeup_own ~first:true d thterm &&
      let last = IArray.length v.lits - 1 in
      wakeup_lit ~first:true d thterm 0 last

  let wakeup d = function
    | Events.Fired.EventValue(_,dom',Lit(thterm,watched,next)) ->
      assert( Value.equal dom dom' );
      ignore (wakeup_lit ~first:false d thterm watched next)
    | Events.Fired.EventValue(_ownr,dom',All thterm) ->
      assert( Value.equal dom dom' );
      (** use this own because the other is the representant *)
      ignore (wakeup_own ~first:false d thterm)
    | _ -> raise UnwaitedEvent


end

module RDaemonPropa = Demon.Fast.Register(DaemonPropa)

module DaemonInit = struct
  let key = Demon.Fast.create "Bool.DaemonInit"

  module Data = DUnit

  let immediate = false
  let throttle = 100
  let wakeup d = function
    | Events.Fired.EventRegSem(thterm,()) ->
      begin try
          let thterm = ThE.coerce_thterm thterm in
          let v = ThE.sem thterm in
          match isnot v with
          | Some node ->
            Delayed.register d node;
            DaemonPropaNot.init d thterm node
          | None ->
            assert (not lazy_propagation);
            IArray.iter (fun (node,_) -> Delayed.register d node) v.lits;
            if DaemonPropa.init d thterm then ()
        (* Delayed.ask_decision d (dec v) *)
        with Exit -> ()
      end
    | _ -> raise UnwaitedEvent

end

module RDaemonInit = Demon.Fast.Register(DaemonInit)


let _true = node_true
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
    | [] -> if topnot then node_true else node_false
    | [node,b] when mulbool topnot b -> _not node
    | [node,_] -> node
    | l ->
      index sem {topnot; lits = IArray.of_list l}
  with Exit -> if topnot then node_false else node_true

let _or_and b l =
  try
    let l = filter (fun f acc ->
        List.fold_left (fun acc e -> f acc (e,b)) acc l) in
    match l with
    | [] -> if b then node_true else node_false
    | [a,b'] -> assert (b == b'); a
    | l ->
      index sem {topnot = b; lits = IArray.of_list l}
  with Exit -> if b then node_false else node_true

let _or  = _or_and false
let _and = _or_and true

let mk_clause m =
  if Node.M.is_empty m then node_false
  else let len = Node.M.cardinal m in
    if len = 1 then
      let node,b = Node.M.choose m in
      if b then _not node else node
    else
      index sem {topnot=false;
                     lits = IArray.of_iter len
                         (fun iter -> Node.M.iter (fun node b -> iter (node,b)) m)}

let _false = node_false

let set_true env pexp node = set_bool env pexp node true

let () =
  Conflict._or := _or;
  Conflict._set_true := set_true

let set_false env pexp node = set_bool env pexp node false

let chobool = Trail.Cho.create_key "Bool.cho"
let make_dec node = Trail.GCho(node,chobool,node)

let converter d f l =
  let of_term t =
    let n = SynTerm.node_of_term t in
    Egraph.Delayed.register d n;
    n
  in
  let node = match f, l with
    | f,([_;_] as args) when Term.equal f Term.or_term ->
      Some (_or (List.map of_term args))
    | f,([_;_] as args) when Term.equal f Term.and_term ->
      Some (_and (List.map of_term args))
    | f,[arg1;arg2] when Term.equal f Term.imply_term ->
      Some (gen false [of_term arg1,true;of_term arg2,false])
    | f,[arg] when Term.equal f Term.not_term ->
      Some (_not (of_term arg))
    | f,[] when Term.equal f Term.true_term ->
      Some _true
    | f,[] when Term.equal f Term.false_term ->
      Some _false
    | _ -> None in
  node

let decvars n =
  if Ty.equal (Node.ty n) ty
  then Some (make_dec n)
  else None

let th_register' with_other_theories env =
  with_other := with_other_theories;
  RDaemonPropaNot.init env;
  RDaemonPropa.init env;
  RDaemonInit.init env;
  Demon.Fast.attach env
    DaemonInit.key [Demon.Create.EventRegSem(sem,())];
  Delayed.register env node_true;
  Delayed.register env node_false;
  SynTerm.register_converter env converter;
  SynTerm.register_decvars env decvars;
  ()

let th_register_alone = th_register' false
let th_register = th_register' true

(** {2 Choice on bool} *)

module ChoBool = struct
  open Conflict

  module OnWhat = Node
  module What = DBool

  let make_decision env node b =
    Debug.dprintf3 print_decision "[Bool] decide %b on %a" b Node.pp node;
    let pexp = Egraph.Delayed.mk_pexp env expprop (ExpDec(node,b)) in
    set_bool env pexp node b

  let choose_decision env node =
    match Egraph.Delayed.get_value env dom node with
    | Some _ -> DecNo
    | None -> DecTodo (fun env -> make_decision env node true) (** why not true? *)

  let key = chobool

end

let () = Conflict.register_cho (module ChoBool)

(*
let choclause = Trail.Cho.create_key "Bool.choclause"

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
*)


(*
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
  if Node.equal node node_true || Node.equal node node_false then s
  else
    let l = ComputeConflict.get_dom t age node dom in
    (* Format.fprintf (Debug.get_debug_formatter ()) *)
    (*   "[get_dom] @[%a@]" *)
    (*   (Pp.list Pp.semi Trail.print_mod_dom) l; *)
    List.fold_left (fun s mod_dom ->
        (* Format.fprintf (Debug.get_debug_formatter ()) *)
        (*   "[rlist] @[%a@]" *)
        (*   Conflict.print_rlist rlist; *)
        let pexp = mod_dom.Trail.modpexp in
        let s = get_con s t (ComputeConflict.get_pexp t pexp conclause) in
        let s,deps =
          ComputeConflict.Equal.one_equal t
            ~from:node
            ~to_:mod_dom.Trail.modcl
            conclause
            s Trail.Deps.empty
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
        Trail.pp_pexp pexp Node.pp cl1 Node.pp cl2
    | DomMerge (pexp,node) ->
      Format.fprintf fmt "DomMerge!(%a,%a)"
        Trail.pp_pexp pexp Node.pp node

(*
  let need_dom t age node =
    if not (Node.equal node node_true || Node.equal node node_false) then
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
    Trail.age -> a Trail.con -> t -> a Conflict.rescon =
    fun t age con exp ->
    let return (s:bool Types.Node.M.t) : a Conflict.rescon =
      match Trail.Con.Eq.eq_type conclause con with
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
*)

module ExpProp = struct

  type t = expprop

  let pp fmt = function
    | ExpBCP  (thterm,node,kind) ->
      Format.fprintf fmt "Bcp(%a,%a = %a;%t)"
        ThE.pp thterm Node.pp (ThE.node thterm) Node.pp node
        (fun _ -> match kind with
           | BCPOwnKnown -> Format.fprintf fmt "Own"
           | BCPLeavesKnown -> Format.fprintf fmt "Leaves"
           | BCP -> ())
    | ExpUp  (thterm,leaf)    ->
      Format.fprintf fmt "Up(%a,%a <- %a)"
        ThE.pp thterm Node.pp (ThE.node thterm) Node.pp leaf
    | ExpDown (thterm,node)    ->
      Format.fprintf fmt "Down(%a,%a,%a ->)"
        ThE.pp thterm Node.pp (ThE.node thterm) Node.pp node
    | ExpNot ((v,clf,clt),b)    ->
      Format.fprintf fmt "Not(%a,%a,%a,%b)"
        Th.pp v Node.pp clf Node.pp clt b
    | ExpDec (n,b) ->
      Format.fprintf fmt "Dec(%a,%b)"
        Node.pp n b


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

  let eq_of_bool n b =
    let nb = node_of_bool b in
    { Conflict.EqCon.l = n; r = nb }


  let analyse_one_to_one t c to_ to_b from_ from_b =
    (** we have
        c: a = b
        we propagated: to_ = to_b
        because      : from_   = not from_b
    *)
    let to_not = node_of_bool to_b in
    let eqs = Conflict.EqCon.split t c to_ to_not in
    let eq = eq_of_bool from_ from_b in
    (eq::eqs)

  let analyse :
    type a. Conflict.Conflict.t ->
    t -> a Trail.Con.t -> a -> Trail.Pcon.t list =
    fun t exp con c ->
      let c = Conflict.Con.Eq.coerce con Conflict.EqCon.key c in
      let eqs = match exp with
        | ExpBCP  (thterm,_,_) when IArray.length (ThE.sem thterm).lits = 1 ->
          raise Impossible
        | ExpBCP  (thterm,propa,kind) ->
          let v = ThE.sem thterm in
          let own = ThE.node thterm in
          let eqs =
            match kind with
            | BCP -> Conflict.EqCon.split t c own propa
            | BCPOwnKnown ->
              let propa_sign = mulbool true (Opt.get (find v propa)) in
              Conflict.EqCon.split t c propa (node_of_bool propa_sign)
            | BCPLeavesKnown ->
              let sign = mulbool false v.topnot in
              Conflict.EqCon.split t c propa (node_of_bool sign)
          in
          let eqs = if kind = BCPOwnKnown then (eq_of_bool own (mulbool true v.topnot))::eqs else eqs in
          fold (fun eqs (node,sign) ->
              if kind <> BCPLeavesKnown && (Node.equal node propa) then eqs
              else (eq_of_bool node (mulbool false sign))::eqs) eqs v
        | ExpUp (thterm,leaf)    ->
          let v = ThE.sem thterm in
          let own = ThE.node thterm in
          analyse_one_to_one t c
            own (mulbool true v.topnot)
            leaf (mulbool true (Opt.get (find v leaf)))
        | ExpDown  (thterm,leaf)    ->
          let v = ThE.sem thterm in
          let own = ThE.node thterm in
          analyse_one_to_one t c
            leaf (mulbool false (Opt.get (find v leaf)))
            own (mulbool false v.topnot)
        | ExpNot  ((_,clfrom,clto),b)->
          analyse_one_to_one t c
            clto b
            clfrom (not b)
        | ExpDec _ ->
          assert false (** absurd: a decision should be the last *)
      in
      Trail.Pcon.map Conflict.EqCon.key eqs

  let key = expprop

  let from_contradiction _ _ =
    assert false (** absurd: never used for contradiction *)
end

let () = Conflict.register_exp(module ExpProp)

let () = Conflict.EqCon.register_apply_learnt ty
    (fun l ->
       let l = List.map (fun {Conflict.EqCon.l;r} ->
           if Node.equal l node_false
           then (r,not true)
           else if Node.equal l node_true
           then (r,not false)
           else if Node.equal r node_false
           then (l,not true)
           else if Node.equal r node_true
           then (l,not false)
           else invalid_arg "Not implemented"
         ) l in
       [gen false l]
    )

(*
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
      'a. 'a Conflict.fold_decisions -> Egraph.Delayed.t -> 'a -> 'a =
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
      | (_true, node) when Node.equal _true node_true ->
        ComputeConflict.set_dec_cho t chobool node;
        GRequested (Node.M.singleton node true)
      | (node, _true) when Node.equal _true node_true ->
        ComputeConflict.set_dec_cho t chobool node;
        GRequested (Node.M.singleton node true)
      | (_false, node) when Node.equal _false node_false ->
        ComputeConflict.set_dec_cho t chobool node;
        GRequested (Node.M.singleton node false)
      | (node, _false) when Node.equal _false node_false ->
        ComputeConflict.set_dec_cho t chobool node;
        GRequested (Node.M.singleton node false)
      | _ ->
        !mk_conequal t clo rcl

  (* let propadom: *)
  (*   type a. ComputeConflict.t -> *)
  (*     Trail.Age.t -> a dom -> Node.t -> a option -> t rescon = *)
  (*     fun t age dom' node bval -> *)
  (*       if ComputeConflict.before_first_dec t age *)
  (*       then GRequested Node.M.empty *)
  (*       else *)
  (*         match Dom.Eq.coerce_type dom dom' with *)
  (*         | Types.Eq -> *)
  (*           if Node.equal node node_true || Node.equal node node_false then *)
  (*             GRequested Node.M.empty *)
  (*           else *)
  (*             GRequested (Node.M.singleton node (Opt.get bval)) *)
end

module EC = Conflict.RegisterCon(ConClause)
*)

(** {2 Interpretations} *)
let () =
  let interp ~interp t =
    let v =
      IArray.fold (fun acc (n,b) ->
          acc ||
          let v = BoolValue.value (BoolValue.coerce_nodevalue (interp n)) in
          if b then not v else v
        ) false t.lits
    in
    let v = if t.topnot then not v else v in
    BoolValue.nodevalue (if v then value_true else value_false)
  in
  Interp.Register.thterm sem interp

let () =
  Interp.Register.id (fun id args ->
      let open Term in
      let is builtin = Term.Id.equal id builtin in
      let (!>) n = BoolValue.value (BoolValue.coerce_nodevalue n) in
      let (!<) b = Some (if b then values_true else values_false) in
      match args with
      | [] when is true_id -> Some values_true
      | [] when is false_id -> Some values_false
      | [a] when is not_id -> !< (not (!> a))
      | l   when is or_id -> !< (List.fold_left (||) false (List.map (!>) l))
      | l   when is and_id -> !< (List.fold_left (&&) true (List.map (!>) l))
      | _ -> None
    )
