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

let debug = Debug.register_info_flag
  ~desc:"for the equality and disequality predicate"
  "disequality"

(** {2 theory term} *)

type t = Node.S.t
(** Is there two elements equal *)

let sem = ThTermKind.create_key (module struct type nonrec t = t let name = "Eq" end)

module Th = struct

  let get_ty v = Node.ty (fst (Node.M.choose v))

  let inv s = not (Node.M.is_empty s || Node.M.is_num_elt 1 s) &&
              let ty = get_ty s in
              (Node.M.for_all (fun e _ -> Ty.equal ty (Node.ty e)) s)

  let only_two s =
    assert (inv s);
    if Node.M.is_num_elt 2 s then
      let enum = Node.M.start_enum s in
      let (cl1,()), enum = Opt.get (Node.M.val_enum enum), Node.M.next_enum enum in
      let (cl2,())       = Opt.get (Node.M.val_enum enum) in
      Some (cl1,cl2)
    else None


  module T : OrderedHashedType with type t = Node.S.t = struct
    include Node.S

    let hash s = Node.S.fold (fun e acc -> Hashcons.combine acc (Node.hash e)) s 29

    let pp fmt s =
      assert (inv s);
      match only_two s with
      | Some (cl1,cl2) ->
        Format.fprintf fmt "%a=@,%a" Node.pp cl1 Node.pp cl2
      | None ->
        Format.fprintf fmt "or=(%a)"
          (Pp.iter1 Node.S.iter Pp.comma Node.pp) s
  end

  include T
  include MkDatatype(T)

  let key = sem

end

module ThE = ThTermKind.Register(Th)

(** {2 disequality domains} *)

module Dis : sig
  type t
  type elt
  val pp: t Pp.pp
  val empty: t
  val of_node: ThE.t -> elt
  val to_node: elt -> ThE.t
  val test_disjoint: (elt -> Trail.Age.t -> unit) -> t -> t -> t
  val disjoint : t -> t -> bool
  val singleton : elt -> Trail.Age.t -> t
  val inter : t -> t -> t
  val is_empty : t -> bool
  val choose : t -> elt * Trail.Age.t
  val iter : (elt -> Trail.Age.t -> unit) -> t -> unit
end = struct
  type t = Trail.Age.t ThE.M.t
  type elt = ThE.t
  let empty = ThE.M.empty
  let pp fmt s =
    Format.fprintf fmt "{%a}"
      (Pp.iter2 ThE.M.iter Pp.comma Pp.colon ThE.pp Trail.Age.pp) s
  let of_node x = x
  let to_node x = x
  let test_disjoint f m1 m2 =
    ThE.M.union (fun k v1 v2 -> assert (Trail.Age.equal v1 v2); f k v1; Some v1) m1 m2
  let disjoint = ThE.M.set_disjoint
  let singleton = ThE.M.singleton
  let is_empty = ThE.M.is_empty
  let inter m1 m2 = ThE.M.inter (fun _ v1 v2 -> assert (Trail.Age.equal v1 v2); Some v2) m1 m2
  let choose m1 = ThE.M.choose m1
  let iter = ThE.M.iter
end

let dom = Dom.create_key (module struct type t = Dis.t let name = "dis" end)

(** For each value key give the value *)
module MValues = ValueKind.MkMap(struct type ('a, _) t = 'a end)

type exp =
  | Merge of Trail.Pexp.t * Node.t * Node.t * Dis.elt * Trail.Age.t
  | SubstUpTrue of ThE.t * Node.t (* e1 *) * Node.t (* e2 *) * Node.t
  | SubstUpFalse of ThE.t * (Node.t * (Dis.t option * unit MValues.t)) list
  | SubstDownTrue of ThE.t
  | SubstDownFalse of ThE.t * Dis.elt
  | Dec of Node.t * Node.t

let exp = Trail.Exp.create_key (module struct type t = exp let name = "Equality" end)

module D = struct
  type t = Dis.t

  let merged (b1:t option) (b2 :t option) =
    match b1,b2 with
    | Some b1, Some b2 -> b1 == b2 (** not Dis.equality *)
    | None, None -> true
    | _ -> false

  let merge d pexp (s1,cl1) (s2,cl2) _ =
    match s1, s2 with
    | None, None -> raise Impossible
    | Some s, None ->
      Egraph.set_dom d dom cl2 s
    | None, Some s ->
      Egraph.set_dom d dom cl1 s
    | Some s1, Some s2 ->
      let s = Dis.test_disjoint (fun i age ->
          let pexp = Egraph.mk_pexp d exp (Merge(pexp,cl1,cl2,i,age)) in
          Egraph.contradiction d pexp) s1 s2 in
      Egraph.set_dom d dom cl1 s;
      Egraph.set_dom d dom cl2 s


  let pp fmt s = Dis.pp fmt s
  let key = dom
end

let () = Dom.register(module D)

let set_dom d _pexp cl s =
  let s = match Egraph.get_dom d dom cl with
    | Some s' ->
      Dis.test_disjoint (fun _ -> assert false) s' s
    | None -> s in
  Egraph.set_dom d dom cl s

let check_sem v cl =
  let own = ThE.node (ThE.index v Bool.ty) in
  Node.equal cl own

(** API *)

let equality cll =
  try
    let fold acc e = Node.S.add_new Exit e acc in
    let s = List.fold_left fold Node.S.empty cll in
    ThE.node (ThE.index s Bool.ty)
  with Exit ->
    Bool._true

let disequality cll = Bool._not (equality cll)

let is_equal t cl1 cl2 = Egraph.is_equal t cl1 cl2
let is_disequal t cl1 cl2 =
  not (Egraph.is_equal t cl1 cl2) &&
  let dom1 = Egraph.get_dom t dom cl1 in
  let dom2 = Egraph.get_dom t dom cl2 in
  match dom1, dom2 with
  | Some s1, Some s2 -> not (Dis.disjoint s1 s2)
  | _ -> false

let new_tag n age =
  let n = Dis.of_node n in
  n, fun () -> Dis.singleton n age (** each instance of this tag must not be == *)

exception Found of Node.t * Node.t

let find_not_disequal d s =
  let is_disequal (dis1,values1) (dis2,values2) =
    (match dis1, dis2 with
     | Some dis1, Some dis2 when not (Dis.disjoint dis1 dis2) -> true
     | _ -> false) ||
    (try
       let fold2_inter (type a) (k:a ValueKind.t) v1 v2 () =
         let module V = (val ValueKind.get k) in
            if not (V.equal v1 v2) then raise Exit
        in
        MValues.fold2_inter {fold2_inter} values1 values2 ();
        false
     with Exit -> true)
  in
  let get_dis_and_values cl =
    Egraph.get_dom d dom cl,
    ValueKind.fold {fold=(fun k acc ->
        match Egraph.get_value d k cl with
        | None -> acc
        | Some v -> MValues.add k v acc)}
      MValues.empty
  in
  assert (Th.inv s);
  let rec inner_loop cl1 s1 enum2 =
    match enum2, s1 with
    | [],_ -> ()
    | (_,d1)::enum2,d2 when is_disequal d1 d2 ->
      inner_loop cl1 s1 enum2
    | (cl2,_)::_,_ ->
      raise (Found (cl1,cl2))
  in
  let rec outer_loop enum1 =
    match enum1 with
    | [] -> ()
    | (cl1,s1)::enum1 ->
      inner_loop cl1 s1 enum1;
      outer_loop enum1 in
  try
    let s = Node.M.fold_left (fun acc cl () ->
        (cl,get_dis_and_values cl)::acc) [] s in
    outer_loop s;
    (** Here we are keeping data because currently
        we are not keeping data for domains globally *)
    `AllDiff s
  with Found (cl1,cl2) ->
    `Found (cl1,cl2)

let norm_set d the =
  let v = ThE.sem the in
  let own = ThE.node the in
  try
    ignore (Node.S.fold_left (fun acc e0 ->
        let e = Egraph.find_def d e0 in
        Node.M.add_change (fun _ -> e0)
            (fun e0 e0' -> raise (Found(e0',e0)))
            e e0 acc)
        Node.M.empty v);
    false
  with Found (e1,e2) ->
    (** TODO remove that and choose what to do. ex: int real *)
    let pexp = Egraph.mk_pexp d exp (SubstUpTrue (the,e1,e2,own)) in
    Bool.set_true d pexp own;
    true

module ChoEquals = struct
  open Conflict

  module OnWhat = ThE

  let key = Trail.Cho.create_key "Equals.cho"

  let make_decision the (cl1,cl2) d =
    Debug.dprintf6 print_decision
      "[Equality] @[decide on merge of %a and %a in %a@]"
      Node.pp cl1 Node.pp cl2 ThE.pp the;
    let pexp = Egraph.mk_pexp d exp (Dec(cl1,cl2)) in
    Egraph.register d cl1;
    Egraph.register d cl2;
    Egraph.merge d pexp cl1 cl2

  let choose_decision d the =
    let v = ThE.sem the in
    let own = ThE.node the in
      Debug.dprintf4 debug "[Equality] @[dec on %a for %a@]"
        Node.pp own ThE.pp the;
      if norm_set d the
      then DecNo
      else
        match find_not_disequal d v with
        | `AllDiff al ->
          let pexp = Egraph.mk_pexp d exp (SubstUpFalse(the,al)) in
          Bool.set_false d pexp own;
          DecNo
        | `Found (cl1,cl2) ->
          DecTodo (make_decision the (cl1,cl2))

end

let () = Conflict.register_cho(module ChoEquals)

let norm_dom d the =
  let v = ThE.sem the in
  let own = ThE.node the in
  if norm_set d the
  then Demon.AliveStopped
  else begin
    Debug.dprintf4 debug "[Equality] @[norm_dom %a %a@]"
      Node.pp own Th.pp v;
    match Bool.is d own with
    | Some false ->
      let age = Trail.Age.succ (Egraph.current_age d) in
      let dis, stag = new_tag the age in
      let pexp =
        Egraph.mk_pexp d exp (SubstDownFalse(the,dis)) in
      Egraph.add_pexp d pexp;
      Node.S.iter (fun cl -> set_dom d pexp cl (stag ())) v;
      Demon.AliveStopped
    | Some true ->
      begin match Th.only_two v with
        | Some (cl1,cl2) ->
          let pexp = Egraph.mk_pexp d exp (SubstDownTrue(the)) in
          Egraph.merge d pexp cl1 cl2; Demon.AliveStopped
        | None ->
          match find_not_disequal d v with
          | `AllDiff al ->
            let pexp = Egraph.mk_pexp d exp (SubstUpFalse(the,al)) in
            Bool.set_false d pexp own; (** contradiction *)
            raise Impossible
          | `Found _ ->
            Demon.AliveStopped
      end
    | None ->
      match find_not_disequal d v with
      | `AllDiff al ->
        let pexp = Egraph.mk_pexp d exp (SubstUpFalse(the,al)) in
        Bool.set_false d pexp own;
        Demon.AliveStopped
      | `Found _ -> (** they are still not proved disequal *)
        Demon.AliveReattached
  end

(** Propagation *)

module DaemonPropa = struct
  let key = Demon.Key.create "Equality.DaemonPropa"

  module Key = Th
  module Data = DUnit
  type info = unit let default = ()

  let immediate = false
  let wakeup d v _ev () =
    norm_dom d (ThE.index v Bool.ty)

end

module RDaemonPropa = Demon.Key.Register(DaemonPropa)

module DaemonInit = struct
  let key = Demon.Key.create "Equality.DaemonInit"

  module Key = DUnit
  module Data = DUnit
  type info = unit let default = ()

  let immediate = true
  let wakeup d () ev () =
    List.iter
      (function Events.Fired.EventRegSem(clsem,()) ->
        begin
          let clsem = ThE.coerce_thterm clsem in
          let v = ThE.sem clsem in
          let own = ThE.node clsem in
          Node.S.iter (Egraph.register d) v;
          let r = norm_dom d clsem in
          begin match r with
          | Demon.AliveReattached ->
            let events = Node.S.fold (fun cl acc ->
              (Demon.Create.EventChange(cl,()))::
              (Demon.Create.EventDom(cl,dom,()))::
              (Demon.Create.EventAnyValue(cl,()))::
              acc
              ) v [] in
            let events = Demon.Create.EventValue(own,Bool.dom,())::events in
            Demon.Key.attach d DaemonPropa.key v events;
            if true (* GenEquality.dodec (Th.get_ty v) *) then begin
              Debug.dprintf4 debug "[Equality] @[ask_dec on %a for %a@]"
                Node.pp own Th.pp v;
              Egraph.register_decision d (Trail.GCho(own,ChoEquals.key,clsem));
            end
          | _ -> ()
          end
        end
      | _ -> raise UnwaitedEvent
      ) ev;
    Demon.AliveReattached

end

module RDaemonInit = Demon.Key.Register(DaemonInit)


(** conflict *)
module HypDis = struct
  open Conflict

  type t = {
    l1 : Node.t;
    l0 : Node.t;
    r0 : Node.t;
    r1 : Node.t;
    disequality : Node.t;
    age : Trail.Age.t;
    }

  let key : t Trail.Hyp.t = Trail.Hyp.create_key "Diff"

  let pp fmt c =
    Format.fprintf fmt "%a=%a≠%a=%a"
      Node.pp c.l1
      Node.pp c.l0
      Node.pp c.r0
      Node.pp c.r1

  let split t c cl1 cl2 =
    if Conflict.age_merge_opt t cl1 c.l1 = None then
      let cl1, cl2 = EqHyp.orient_split t {l=c.r0;r=c.r1} cl1 cl2 in
      (Trail.Phyp.phyp key {c with r1 = cl1})::(EqHyp.create_eq cl2 c.r1)
    else
      let cl1, cl2 = EqHyp.orient_split t {l=c.l0;r=c.l1} cl1 cl2 in
      (Trail.Phyp.phyp key {c with l1 = cl1})::(EqHyp.create_eq cl2 c.l1)


  let useful_nodes c =
    Bag.list [c.l1;c.l0;c.r1;c.r0]

  let levels t c =
    let l = Levels.empty in
    let l = Levels.add t (Conflict.age_merge t c.l1 c.l0) l in
    let l = Levels.add t (Conflict.age_merge t c.r1 c.r0) l in
    let l = Levels.add t c.age l in
    l

  let apply_learnt c =
    let n, par = EqHyp.apply_learnt {l=c.l1;r=c.r1} in
    n, neg_parity par

  let create_diff_far t cl1 cl2 i age =
    let find_origin v cl =
      Node.S.fold_left (fun acc cl0 ->
          match acc with
          | Some _ -> acc
          | None ->
            match Conflict.age_merge_opt t cl cl0 with
            | Some _ -> Some cl0
            | None -> None) None v
    in
    let the = Dis.to_node i in
    let v = ThE.sem the in
    let cl1_0 = Opt.get (find_origin v cl1) in
    let cl2_0 = Opt.get (find_origin v cl2) in
    let diff = Trail.Phyp.phyp key {l1=cl1;l0=cl1_0;r0=cl2_0;r1=cl2;disequality=ThE.node the; age} in
    diff

    let create_diff_near t cl1 cl2 i age =
    let find_origin v cl =
      Node.S.fold_left (fun acc cl0 ->
          match acc with
          | Some _ -> acc
          | None ->
            match Conflict.age_merge_opt t cl cl0 with
            | Some _ -> Some cl0
            | None -> None) None v
    in
    let the = Dis.to_node i in
    let v = ThE.sem the in
    let cl1_0 = Opt.get (find_origin v cl1) in
    let cl2_0 = Opt.get (find_origin v cl2) in
    let diff = Trail.Phyp.phyp key {l1=cl1_0;l0=cl1_0;r0=cl2_0;r1=cl2_0;disequality=ThE.node the; age} in
    diff, (Trail.Phyp.phyp EqHyp.key {l=cl1_0;r=cl2_0})

end

let () = Conflict.register_hyp(module HypDis)

module Exp = struct
  open Conflict

  type t = exp

  let pp fmt = function
    | Merge  (pexp,cl1,cl2,i,_)   ->
      Format.fprintf fmt "Merge!(%a,%a,%a,%a)"
        pp_pexp pexp Node.pp cl1 Node.pp cl2 ThE.pp (Dis.to_node i)
    | SubstUpTrue    (v,e1,e2,cl)   -> (** two are equals *)
      Format.fprintf fmt "SubstUpTrue(%a,%a,%a,%a)"
        ThE.pp v Node.pp e1 Node.pp e2 Node.pp cl
    | SubstUpFalse   (the,_)   ->
      Format.fprintf fmt "SubstUpFalse(%a)" ThE.pp the
    | SubstDownTrue  (the)   ->
      Format.fprintf fmt "SubstDownTrue(%a)" ThE.pp the
    | SubstDownFalse (v,i)   ->
      Format.fprintf fmt "SubstDownFalse(%a,%a)"
        ThE.pp v ThE.pp (Dis.to_node i)
    | Dec (n1,n2) ->
      Format.fprintf fmt "Dec(%a,%a)"
        Node.pp n1 Node.pp n2

  let analyse t e phyp =
    match e with
    | SubstUpTrue    (v,e1,e2,_)   -> (** two are equals *)
      let own = ThE.node v in
      let lhyp = Conflict.split t phyp own Bool._true in
      let phyp = Trail.Phyp.phyp EqHyp.key {l=e1;r=e2} in
      phyp::lhyp
    | SubstUpFalse   (v,al)   ->
      let own = ThE.node v in
      let lhyp = Conflict.split t phyp own Bool._false in
      let al = CCList.diagonal al in
      let fold lhyp ((e1,(dis1,val1)),(e2,(dis2,val2))) =
        let diff_value () = (** different values *)
          let fold2_inter (type a) (k:a ValueKind.t) v1 v2 acc =
            let (module V) = ValueKind.get k in
            if not (V.equal v1 v2) then
              (EqHyp.create_eq e1 (Node.index_value k v1 (Node.ty e1))) @
              (EqHyp.create_eq e2 (Node.index_value k v2 (Node.ty e2))) @
              acc
            else acc
          in
          let lhyp' = MValues.fold2_inter {fold2_inter} val1 val2 lhyp in
          assert (not (lhyp == lhyp')); (** One is different *)
          lhyp'
        in
        match dis1, dis2 with
        | Some dis1, Some dis2 ->
          let dis = Dis.inter dis1 dis2 in
          if Dis.is_empty dis
          then diff_value ()
          else
            (** choose the oldest? *)
            let d,age = Dis.choose dis in
            let diff = HypDis.create_diff_far t e1 e2 d age in
            diff::lhyp
        | _ -> diff_value ()
      in
      List.fold_left fold lhyp al
    | SubstDownTrue  (the)   -> begin
      let v = ThE.sem the in
      match Node.S.elements v with
      | [a;b] ->
        let lhyp = Conflict.split t phyp a b in
        (EqHyp.create_eq (ThE.node the) Bool._true)@lhyp
      | _ -> raise Impossible
    end
    | SubstDownFalse (the,_)   ->
      let Trail.Phyp.Phyp(hyp,c,_) = phyp in
      let c = Hyp.Eq.coerce hyp HypDis.key c in
      let lhyp = [] in
      let lhyp = (EqHyp.create_eq c.l1 c.l0)@lhyp in
      let lhyp = (EqHyp.create_eq c.r1 c.r0)@lhyp in
      let lhyp = (EqHyp.create_eq (ThE.node the) Bool._false)@lhyp in
      lhyp
    | Dec(n1,n2) ->
      let lhyp = Conflict.split t phyp n1 n2 in
      let eq = EqHyp.create_eq ~dec:() n1 n2 in
      eq@lhyp
    | Merge(pexp,cl1,cl2,i,age) ->
      assert (pexp == Trail.pexp_fact);
      (** only for bool currently *)
      let cl2' = if Node.equal cl2 Bool._true then Bool._false else Bool._true in
      let lhyp = Conflict.split t phyp cl1 cl2' in
      let diff = HypDis.create_diff_far t cl1 cl2 i age in
      diff::lhyp

  let key = exp

  let far_disequality = Debug.register_flag "far-disequality"
      ~desc:"Instead of explaining conflict with distinct near the disequality, explain it far from it"

  let from_contradiction t = function
    | Merge(pexp,cl1,cl2,i,age) ->
      if Debug.test_flag far_disequality then
        let lhyp = Conflict.analyse t pexp (Trail.Phyp.phyp EqHyp.key {l=cl1;r=cl2}) in
        let diff = HypDis.create_diff_far t cl1 cl2 i age in
        diff::lhyp
      else
        let diff, eq = HypDis.create_diff_near t cl1 cl2 i age in
        let lhyp = Conflict.analyse t pexp eq in
        diff::lhyp
    | _ -> raise Impossible

end


let () = Conflict.register_exp(module Exp)


(** ITE *)
type ite = {cond: Node.t; then_: Node.t; else_: Node.t}

module ITE = struct

  module TITE = struct
    type t = ite
    let equal x y = Node.equal x.cond y.cond &&
                    Node.equal x.then_ y.then_ &&
                    Node.equal x.else_ y.else_
    let compare x y =
      let c = Node.compare x.cond y.cond in
      if c != 0 then c
      else let c = Node.compare x.then_ y.then_ in
        if c != 0 then c
        else Node.compare x.else_ y.else_
    let hash x =
      Hashcons.combine2 (Node.hash x.cond) (Node.hash x.then_) (Node.hash x.else_)

    let pp fmt x =
      Format.fprintf fmt "ite(%a,%a,%a)"
        Node.pp x.cond Node.pp x.then_ Node.pp x.else_
  end

  include TITE
  include MkDatatype(TITE)

  let key = ThTermKind.create_key "ite"

end

module EITE = ThTermKind.Register(ITE)

let ite cond then_ else_ =
  let ty1 = Node.ty then_ in
  let ty2 = Node.ty else_ in
  assert (Ty.equal ty1 ty2);
  Node.index_sem ITE.key { cond; then_; else_} ty1

let expite : (EITE.t * bool) Trail.Exp.t =
  Trail.Exp.create_key "Ite.exp"

module DaemonPropaITE = struct
  let key = Demon.Fast.create "ITE.propa"

  module Data = EITE

  let simplify d the b =
    let v = EITE.sem the in
    let own = EITE.node the in
    let branch = if b then v.then_ else v.else_ in
    let pexp = Egraph.mk_pexp d expite (the,b) in
    Egraph.register d branch;
    Egraph.merge d pexp own branch

  let immediate = false
  let throttle = 100
  let wakeup d = function
    | Events.Fired.EventValue(cond,dom,clsem) ->
      assert (ValueKind.equal dom Bool.dom);
      let v = EITE.sem clsem in
      assert (Egraph.is_equal d cond v.cond);
      begin match Bool.is d v.cond with
        | None -> assert false
        | Some b -> simplify d clsem b
      end
    | _ -> raise UnwaitedEvent

end

module RDaemonPropaITE = Demon.Fast.Register(DaemonPropaITE)

module DaemonInitITE = struct
  let key = Demon.Fast.create "ITE.init"

  module Key = DUnit
  module Data = DUnit

  let immediate = false
  let throttle = 100
  let wakeup d = function
    | Events.Fired.EventRegSem(clsem,()) ->
      begin
        let clsem = EITE.coerce_thterm clsem in
        let v = EITE.sem clsem in
        let own = EITE.node clsem in
        match Bool.is d v.cond with
        | Some b ->
          DaemonPropaITE.simplify d clsem b
        | None ->
          let clsem = EITE.index v (Node.ty own) in
          assert (Node.equal (EITE.node clsem) own);
          Egraph.register d v.cond;
          Egraph.register d v.then_;
          Egraph.register d v.else_;
          Egraph.register_decision d (Trail.GCho(v.cond,Bool.chobool,v.cond));
          let events = [Demon.Create.EventValue(v.cond,Bool.dom,clsem)] in
          Demon.Fast.attach d DaemonPropaITE.key events
    end
    | _ -> raise UnwaitedEvent

end

module RDaemonInitITE = Demon.Fast.Register(DaemonInitITE)

module ExpITE = struct
  open Conflict

  type t = EITE.t * bool
  let key = expite

  let pp fmt (ite,b) =
    Format.fprintf fmt "(%a,%b)" EITE.pp ite b

  let analyse :
      Conflict.t ->
    (* Trail.age -> *) t -> Trail.Phyp.t -> Trail.Phyp.t list =
    fun t (the,b) hyp ->
      let v = EITE.sem the in
      let own = EITE.node the in
      let lhyp = Conflict.split t hyp own (if b then v.then_ else v.else_) in
      let phyp = EqHyp.create_eq v.cond (if b then Bool._true else Bool._false) in
      phyp@lhyp

  let from_contradiction _ _ = raise Impossible

end

let () = Conflict.register_exp(module ExpITE)

(** {2 Link between diff and values} *)
(** If can't be a value it they share a diff tag, are different *)

(** Give for a node the values that are different *)
let iter_on_value_different
    (type a)
    (type b)
    ((module Val): (module Witan_core.ValueKind.Registered with type s = a and type t = b))
    ~they_are_different
    (d:Egraph.t)
    (own:Node.t) =
  let dis = Opt.get_def Dis.empty (Egraph.get_dom d dom own) in
  let iter elt age =
    let iter n =
      if not (Egraph.is_equal d own n) then
        match Egraph.get_value d Val.key n with
        | None -> ()
        | Some v ->
        let pexp =
          Egraph.mk_pexp d exp (Merge(Trail.pexp_fact,own,n,elt,age)) in
        they_are_different pexp n v
    in
    Node.S.iter iter (ThE.sem (Dis.to_node elt))
  in
  Dis.iter iter dis

(** Give for a value the nodes that are different *)
let init_diff_to_value (type a) (type b)
    ?(already_registered=([]: b list))
    d0
    ((module Val): (module Witan_core.ValueKind.Registered with type s = a and type t = b))
    ~(they_are_different:(Egraph.t -> Trail.Pexp.t -> Node.t -> a -> unit)) =

  let propagate_diff d v =
    let own = Val.node v in
    let dis = Opt.get_def Dis.empty (Egraph.get_dom d dom own) in
    let iter elt age =
      let iter n =
        if not (Egraph.is_equal d own n) then
          let pexp =
            Egraph.mk_pexp d exp (Merge(Trail.pexp_fact,n,Val.node v,elt,age)) in
          they_are_different d pexp n (Val.value v)
      in
      Node.S.iter iter (ThE.sem (Dis.to_node elt))
    in
    Dis.iter iter dis
  in
  let key = Demon.Fast.create (Format.asprintf "DiffToValue.%a" ValueKind.pp Val.key)
  in
  let module D = Demon.Fast.Register(struct
      module Data = Val
      let key = key
      let throttle = 100
      let immediate = false

      let wakeup d = function
        | Events.Fired.EventDom(_,_,v) ->
          propagate_diff d v
        | _ -> raise Impossible
    end)
  in

  let init d (v:Val.t) =
    propagate_diff d v;
    Demon.Fast.attach d key [Demon.Create.EventDom(Val.node v,dom,v)]
  in
  D.init d0;
  Demon.Fast.register_init_daemon_value
    ~name:(Format.asprintf "DiffToValue.Init.%a" ValueKind.pp Val.key)
    (module Val)
    init
    d0;
  List.iter (init d0) already_registered

(** {3 For booleans} *)
(* Since the module Bool is linked before *)

let bool_init_diff_to_value d =
  init_diff_to_value
    d (module Bool.BoolValue)
    ~they_are_different:(fun d pexp n b ->
        if not b then Bool.set_true d pexp n
        else Bool.set_false d pexp n
      )
    ~already_registered:[Bool.value_true;Bool.value_false]

(** {2 Interpretations} *)
let () =
  let interp ~interp t =
    try
      let fold acc e = Value.S.add_new Exit (interp e) acc in
      let _ = Node.S.fold_left fold Value.S.empty t in
      Bool.values_false
    with Exit ->
      Bool.values_true
  in
  Interp.Register.thterm sem interp

let () =
  let interp ~interp (t:ITE.t) =
    let c = Bool.BoolValue.value (Bool.BoolValue.coerce_nodevalue (interp t.cond)) in
    if c then interp t.then_ else interp t.else_
  in
  Interp.Register.thterm ITE.key interp

let () =
  Interp.Register.id (fun id args ->
      let open Term in
      let is builtin = Term.Id.equal id builtin in
      let (!>) n = Bool.BoolValue.value (Bool.BoolValue.coerce_nodevalue n) in
      let (!<) b = Some (if b then Bool.values_true else Bool.values_false) in
      match args with
      | [a;b] when is equal_id || is equiv_id -> !< (Value.equal a b)
      | [c;a;b] when is ite_id -> Some (if (!> c) then a else b)
      | _   when is_distinct_id id -> begin
          try
            let fold acc v = Value.S.add_new Exit v acc in
            let _ = List.fold_left fold Value.S.empty args in
            Some Bool.values_false
          with Exit ->
            Some Bool.values_true
        end
      | _ -> None
    )


let converter d f l =
  let of_term t =
    let n = SynTerm.node_of_term t in
    Egraph.register d n;
    n
  in
  let node = match f, l with
    | f,(_::([_;_] as args)) when Term.equal f Term.equal_term ||
                             Term.equal f Term.equiv_term ->
      Some (equality (List.map of_term args))
    | f,(_::args) when Term.is_distinct_term f ->
      Some (disequality (List.map of_term args))
    | f,[_;c;a;b] when Term.equal f Term.ite_term ->
      Some (ite (of_term c) (of_term a) (of_term b))
    | _, _ -> None in
  node

let () = Conflict._equality := (fun a b -> equality [a;b])


let th_register env =
  RDaemonPropa.init env;
  RDaemonInit.init env;
  RDaemonPropaITE.init env;
  RDaemonInitITE.init env;
  Demon.Key.attach env
    DaemonInit.key () [Demon.Create.EventRegSem(sem,())];
  Demon.Fast.attach env
    DaemonInitITE.key [Demon.Create.EventRegSem(ITE.key,())];
  SynTerm.register_converter env converter;
  bool_init_diff_to_value env;
  ()
