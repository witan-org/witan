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

open Stdlib
open Std
open Witan_core

let lazy_propagation = false

let debug = Debug.register_info_flag
  ~desc:"for the boolean theory"
  "bool"

let ty = Term._Prop
let dom : bool ValueKind.t = ValueKind.create_key "bool"

module BoolValue = ValueKind.Register(struct
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

let is env node = Egraph.get_value env dom node
let is_true  env node = Node.equal node node_true || is env node = Some true
let is_false env node = Node.equal node node_false || is env node = Some false
let is_unknown env node = is env node = None

let set_bool env pexp node b =
  Egraph.merge env pexp node
    (if b then node_true else node_false)

type t =
  { topnot: bool;
    lits: (Node.t * bool) IArray.t;
  }

let sem : t ThTermKind.t = ThTermKind.create_key "Prop"

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
      match Egraph.get_value d dom node with
      | Some _ -> ()
      | None -> raise (Found (node,b))
    ) l

  let _bcp d l absorbent =
    try
      let res = IArray.fold (fun acc node ->
        match Egraph.get_value d dom node, acc with
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

module ThE = ThTermKind.Register(Th)

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
      assert (ValueKind.equal dom dom');
      begin match Egraph.get_value d dom node with
        | None -> raise Impossible
        | Some b ->
          let pexp = Egraph.mk_pexp d expprop (ExpNot(x,not b)) in
          set_bool d pexp ncl (not b)
      end;
    | _ -> raise UnwaitedEvent

  let init d thterm node =
    let v = ThE.sem thterm in
    let own = ThE.node thterm in
    match is d own with
    | Some b ->
      let pexp = Egraph.mk_pexp d expprop (ExpNot((v,own,node),not b)) in
      set_bool d pexp node (not b)
    | None ->
      match is d node with
      | Some b ->
        let pexp = Egraph.mk_pexp d expprop
            (ExpNot((v,node,own),not b)) in
        set_bool d pexp own (not b)
      | None ->
        let events = [Demon.Create.EventValue(own,dom,(v,own,node));
                      Demon.Create.EventValue(node,dom,(v,node,own))] in
        Demon.Fast.attach d key events

end

module RDaemonPropaNot = Demon.Fast.Register(DaemonPropaNot)

module DaemonPropa = struct
  type watcher = (int,int) Context.Ref2.t

  type d =
    | Lit of ThE.t (* prop *) * int (* watched *) * watcher
  | All of ThE.t

  let key = Demon.Fast.create "Bool.DaemonPropa"

  module Data = struct
    type t = d
    let pp fmt = function
      | Lit (thterm,i,w) ->
        let w,n = Context.Ref2.get w in
        Format.fprintf fmt "Lit(%a,%i(%i,%i),%a)" ThE.pp thterm i w n
          Node.pp (ThE.node thterm)
      | All thterm -> Format.fprintf fmt "All(%a)" ThE.pp thterm
  end

  let immediate = false
  let throttle = 100

  let wakeup_lit d thterm watched watcher =
    let v = ThE.sem thterm in
    let own = ThE.node thterm in
    let pexp exp = Egraph.mk_pexp d expprop exp in
    let set_dom_up_true d own leaf _ =
      let b = (not v.topnot) in
      match Egraph.get_value d dom own with
      | Some b' when b' == b -> ()
      | _ -> set_bool d (pexp (ExpUp(thterm,leaf))) own b in
    let merge_bcp node sign =
      Debug.dprintf2 debug "[Bool] @[merge_bcp %a@]" Node.pp node;
      match Egraph.get_value d dom own with
      | Some b' ->
        let pexp = if (mulbool b' v.topnot)
          then pexp (ExpBCP(thterm,node,BCPOwnKnown))
          else pexp (ExpDown(thterm,node))
        in
        let b = mulbool sign (mulbool b' v.topnot) in
        set_bool d pexp node b
      | None -> (** merge *)
        match Egraph.get_value d dom node with
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
          else Egraph.merge d (pexp (ExpBCP(thterm,node,BCP))) own node in
    let rec find_watch dir pos bound =
      assert (dir == 1 || dir == -1);
      if pos == bound
      then
        let node,sign = IArray.get v.lits pos in
        (merge_bcp node sign; raise Exit)
      else
        let node,sign = IArray.get v.lits pos in
        match Egraph.get_value d dom node with
        | None -> node,pos
        | Some b when mulbool b sign (** true absorbent of or *) ->
          set_dom_up_true d own node b; raise Exit
        | Some _ (** false *) -> find_watch dir (dir+pos) bound
    in
    try
      let w1, w2 = Context.Ref2.get watcher in
      if w1 = -1 (** already done *)
      then false
      else begin
        assert (watched = w1 || watched = w2);
        assert (w1 < w2);
        let dir,bound = if watched = w1 then 1,w2 else -1,w1 in
        let clwatched, watched = find_watch dir watched bound in
        if dir = 1
        then Context.Ref2.set1 watcher watched
        else Context.Ref2.set2 watcher watched;
        Demon.Fast.attach d key
          [Demon.Create.EventValue(clwatched,dom,
                                   Lit(thterm,watched,watcher))] ;
        true
      end
    with Exit ->
      Context.Ref2.set watcher (-1) (-1);
      false

  let wakeup_own d thterm =
    let v = ThE.sem thterm in
    let own = ThE.node thterm in
    let pexp exp = Egraph.mk_pexp d expprop exp in
    begin match Egraph.get_value d dom own with
    | None -> (* only during init *)
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
    wakeup_own d thterm &&
    let last = IArray.length v.lits - 1 in
    assert (last <> 0);
    let watcher = Context.Ref2.create (Egraph.context d) 0 last in
    wakeup_lit d thterm 0 watcher &&
    wakeup_lit d thterm last watcher

  let wakeup d = function
    | Events.Fired.EventValue(_,dom',Lit(thterm,watched,next)) ->
      assert( ValueKind.equal dom dom' );
      ignore (wakeup_lit d thterm watched next)
    | Events.Fired.EventValue(_ownr,dom',All thterm) ->
      assert( ValueKind.equal dom dom' );
      (** use this own because the other is the representant *)
      ignore (wakeup_own d thterm)
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
            Egraph.register d node;
            DaemonPropaNot.init d thterm node
          | None ->
            assert (not lazy_propagation);
            IArray.iter (fun (node,_) -> Egraph.register d node) v.lits;
            if DaemonPropa.init d thterm then ()
        (** we could register a decision here, if we want to do
            decision on any boolean operations not only variable *)
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
  let gen_or l =
    let l = List.map (function
        | (n,Conflict.Pos) -> (n,false)
        | (n,Conflict.Neg) -> (n,true)) l in
    gen false l in
  Conflict._or := gen_or;
  Conflict._set_true := set_true;
  Conflict._is_true := is_true

let set_false env pexp node = set_bool env pexp node false

let chobool = Trail.Cho.create_key "Bool.cho"
let make_dec node = Trail.GCho(node,chobool,node)

let converter d f l =
  let of_term t =
    let n = SynTerm.node_of_term t in
    Egraph.register d n;
    n
  in
  let node = match f, l with
    | f,args when Term.is_or_term f ->
      Some (_or (List.map of_term args))
    | f,args when Term.is_and_term f ->
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

let th_register env =
  RDaemonPropaNot.init env;
  RDaemonPropa.init env;
  RDaemonInit.init env;
  Demon.Fast.attach env
    DaemonInit.key [Demon.Create.EventRegSem(sem,())];
  Egraph.register env node_true;
  Egraph.register env node_false;
  SynTerm.register_converter env converter;
  SynTerm.register_decvars env decvars;
  ()

(** {2 Choice on bool} *)

module ChoBool = struct
  open Conflict

  module OnWhat = Node
  module What = DBool

  let make_decision env node b =
    Debug.dprintf3 print_decision "[Bool] decide %b on %a" b Node.pp node;
    let pexp = Egraph.mk_pexp env expprop (ExpDec(node,b)) in
    set_bool env pexp node b

  let choose_decision env node =
    match Egraph.get_value env dom node with
    | Some _ -> DecNo
    | None -> DecTodo (fun env -> make_decision env node true) (** why not true? *)

  let key = chobool

end

let () = Conflict.register_cho (module ChoBool)

(** {2 Conflict} *)

(** We could use instead directly EqHyp, but it gives an example of a
   simple conflict other than EqHyp *)
module HypProp = struct
  type t = (Node.t * bool)

  let pp fmt (n,b) =
    if b
    then Format.fprintf fmt "¬%a" Node.pp n
    else Node.pp fmt n

  let key : t Trail.Hyp.t = Trail.Hyp.create_key "hypprop"

  let apply_learnt (n,b) = (n,if b then Conflict.Neg else Conflict.Pos)

  let node_of_sign b = (node_of_bool (mulbool true b))

  let levels t (n,b) =
    let levels = Conflict.Levels.empty in
    let age = Conflict.Conflict.age_merge t n (node_of_sign b) in
    Conflict.Levels.add t age levels

  let useful_nodes (n,_) = Bag.elt n

  let split t (n,b) a' b' =
    let l', r' = Conflict.EqHyp.split t {l=n;r=node_of_sign b} a' b' in
    (match l' with
     | None -> []
     | Some r -> [Trail.Phyp.phyp Conflict.EqHyp.key {l=n; r}])
    @
    (match r' with
     | None -> []
     | Some l -> [Trail.Phyp.phyp key (l,b)])

end

let () = Conflict.register_hyp (module HypProp)

(** {2 Explanation} *)

module ExpProp = struct
  open Conflict
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

  let eq_of_bool ?dec n b =
    Trail.Phyp.phyp ?dec HypProp.key (n,not b)

  let analyse_one_to_one ?dec t phyp to_ to_b from_ from_b =
    (** we have
        c: a = b
        we propagated: to_ = to_b
        because      : from_   = from_b
    *)
    let to_not = node_of_bool to_b in
    let eqs = Conflict.split t phyp to_ to_not in
    let eq = eq_of_bool ?dec from_ from_b in
    Debug.dprintf10 debug "clfrom:%a from_b:%b clto:%a to_b:%b eqs:%a eq:%a"
      Node.pp from_
      from_b
      Node.pp to_
      to_b
      (Pp.list Pp.comma pp_phyp) eqs
      pp_phyp eq;
    (eq::eqs)

  let analyse :
    Conflict.t ->
    t -> Trail.Phyp.t -> Trail.Phyp.t list =
    fun t exp phyp ->
      match exp with
      | ExpBCP  (thterm,_,_) when IArray.length (ThE.sem thterm).lits = 1 ->
        raise Impossible
      | ExpBCP  (thterm,propa,kind) ->
        let v = ThE.sem thterm in
        let own = ThE.node thterm in
        let eqs =
          match kind with
          | BCP -> Conflict.split t phyp own propa
          | BCPOwnKnown ->
            let propa_sign = mulbool true (Opt.get (find v propa)) in
            Conflict.split t phyp propa (node_of_bool propa_sign)
          | BCPLeavesKnown ->
            let sign = mulbool false v.topnot in
            Conflict.split t phyp propa (node_of_bool sign)
        in
        let eqs = if kind = BCPOwnKnown then (eq_of_bool own (mulbool true v.topnot))::eqs else eqs in
        fold (fun eqs (node,sign) ->
            if kind <> BCPLeavesKnown && (Node.equal node propa) then eqs
            else (eq_of_bool node (mulbool false sign))::eqs) eqs v
      | ExpUp (thterm,leaf)    ->
        let v = ThE.sem thterm in
        let own = ThE.node thterm in
        analyse_one_to_one t phyp
          own (mulbool true v.topnot)
          leaf (mulbool true (Opt.get (find v leaf)))
      | ExpDown  (thterm,leaf)    ->
        let v = ThE.sem thterm in
        let own = ThE.node thterm in
        analyse_one_to_one t phyp
          leaf (mulbool false (Opt.get (find v leaf)))
          own (mulbool false v.topnot)
      | ExpNot  ((_,clfrom,clto),b)->
        analyse_one_to_one t phyp
            clto b
            clfrom (not b)
      | ExpDec (cl,b) ->
        analyse_one_to_one ~dec:() t phyp
          cl b
          cl b

  let key = expprop

  let from_contradiction _ _ =
    assert false (** absurd: never used for contradiction *)
end

let () = Conflict.register_exp(module ExpProp)

let () =
  let parity_of_bool b = if b then Conflict.Neg else Conflict.Pos in
  Conflict.EqHyp.register_apply_learnt ty
    (fun {Conflict.EqHyp.l;r} ->
       if Node.equal l node_false
       then (r,parity_of_bool true)
       else if Node.equal l node_true
       then (r,parity_of_bool false)
       else if Node.equal r node_false
       then (l,parity_of_bool true)
       else if Node.equal r node_true
       then (l,parity_of_bool false)
       else !Conflict._equality l r, Pos
    )

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

let default_value = true

let () =
  Interp.Register.model ty (fun d n ->
      let v = Egraph.get_value d dom n in
      let v = Witan_popop_lib.Opt.get_def default_value v in
      let v = if v then values_true else values_false in
      v)




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
      | l   when is_or_id id -> !< (List.fold_left (||) false (List.map (!>) l))
      | l   when is_and_id id -> !< (List.fold_left (&&) true (List.map (!>) l))
      | _ -> None
    )
