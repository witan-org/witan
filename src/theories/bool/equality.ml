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

let debug = Debug.register_info_flag
  ~desc:"for the equality and disequality predicate"
  "disequality"

(** {2 theory term} *)

type t = Node.S.t
(** Is there two elements equal *)

let sem : t Sem.t = Sem.create_key "Eq"

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

module ThE = Sem.Register(Th)

(** {2 disequality domains} *)

module Dis : sig
  include Witan_popop_lib.Extset.S
  val pp: t Pp.pp
  val of_node: ThE.t -> elt
  val to_node: elt -> ThE.t
end = struct
  include ThE.S
  let pp fmt s =
    Format.fprintf fmt "{%a}"
      (Pp.iter1 ThE.S.iter Pp.semi ThE.pp) s
  let of_node x = x
  let to_node x = x
end

let dom : Dis.t Dom.t = Dom.create_key "dis"

module D = struct
  type t = Dis.t

  let merged (b1:t option) (b2 :t option) =
    match b1,b2 with
    | Some b1, Some b2 -> b1 == b2 (** not Dis.equality *)
    | None, None -> true
    | _ -> false

  type expmerge =
  | Merge of Trail.Pexp.t * Node.t * Node.t * Dis.elt

  let expmerge : expmerge Trail.Exp.t =
    Trail.Exp.create_key "Equality.merge"

  let merge d pexp (s1,cl1) (s2,cl2) _ =
    match s1, s2 with
    | None, None -> raise Impossible
    | Some s, None ->
      Delayed.set_dom_premerge d dom cl2 s
    | None, Some s ->
      Delayed.set_dom_premerge d dom cl1 s
    | Some s1, Some s2 ->
      let s = Dis.M.union (fun i () ->
          let pexp = Delayed.mk_pexp d expmerge (Merge(pexp,cl1,cl2,i)) in
          Delayed.contradiction d pexp) s1 s2 in
      Delayed.set_dom_premerge d dom cl1 s;
      Delayed.set_dom_premerge d dom cl2 s


  let pp fmt s = Dis.pp fmt s
  let key = dom
end

let () = Dom.register(module D)

let set_dom d pexp cl s =
  let s = match Delayed.get_dom d dom cl with
    | Some s' ->
      Dis.M.union (fun _i () -> assert false) s' s
    | None -> s in
  Delayed.set_dom d pexp dom cl s

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

let is_equal t cl1 cl2 = Delayed.is_equal t cl1 cl2
let is_disequal t cl1 cl2 =
  not (Delayed.is_equal t cl1 cl2) &&
  let dom1 = Delayed.get_dom t dom cl1 in
  let dom2 = Delayed.get_dom t dom cl2 in
  match dom1, dom2 with
  | Some s1, Some s2 -> not (Dis.disjoint s1 s2)
  | _ -> false

let new_tag n =
  let n = Dis.of_node n in
  n, fun () -> Dis.singleton n (** each instance of this tag must not be == *)

exception Found of Node.t * Node.t

let find_not_disequal d s =
  assert (Th.inv s);
  let rec inner_loop cl1 s1 enum2 =
    match enum2, s1 with
    | [],_ -> ()
    | (cl2,None)::_,_ | (cl2,_)::_, None ->
      raise (Found (cl1,cl2))
    | (cl2,Some s2)::_, Some s1 when Dis.disjoint s1 s2 ->
      raise (Found (cl1,cl2))
    | _::enum2, _ -> inner_loop cl1 s1 enum2 in
  let rec outer_loop enum1 =
    match enum1 with
    | [] -> ()
    | (cl1,s1)::enum1 ->
      inner_loop cl1 s1 enum1;
      outer_loop enum1 in
  try
    let s = Node.M.fold_left (fun acc cl () ->
        (cl,Delayed.get_dom d dom cl)::acc) [] s in
    outer_loop s;
    (** Here we are keeping data because currently
        we are not keeping data for domains globally *)
    `AllDiff s
  with Found (cl1,cl2) ->
    `Found (cl1,cl2)

type expsubst =
| SubstUpTrue of ThE.t * Node.t (* e1 *) * Node.t (* e2 *) * Node.t
| SubstUpFalse of ThE.t * (Node.t * Dis.t option) list
| SubstDownTrue of ThE.t
| SubstDownFalse of ThE.t * Dis.elt

let expsubst : expsubst Trail.Exp.t =
  Trail.Exp.create_key "Equality.subst"

let norm_set d the =
  let v = ThE.sem the in
  let own = ThE.node the in
  try
    ignore (Node.S.fold_left (fun acc e0 ->
        let e = Delayed.find_def d e0 in
        Node.M.add_change (fun _ -> e0)
            (fun e0 e0' -> raise (Found(e0',e0)))
            e e0 acc)
        Node.M.empty v);
    false
  with Found (e1,e2) ->
    (** TODO remove that and choose what to do. ex: int real *)
    let pexp = Delayed.mk_pexp d expsubst (SubstUpTrue (the,e1,e2,own)) in
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
    let pexp = Trail.pexp_fact in
    Delayed.register d cl1;
    Delayed.register d cl2;
    Delayed.merge d pexp cl1 cl2

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
          let pexp = Delayed.mk_pexp d expsubst (SubstUpFalse(the,al)) in
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
      let dis, stag = new_tag the in
      let pexp =
        Delayed.mk_pexp d expsubst (SubstDownFalse(the,dis)) in
      Node.S.iter (fun cl -> set_dom d pexp cl (stag ())) v;
      Demon.AliveStopped
    | Some true ->
      begin match Th.only_two v with
        | Some (cl1,cl2) ->
          let pexp = Delayed.mk_pexp d expsubst (SubstDownTrue(the)) in
          Delayed.merge d pexp cl1 cl2; Demon.AliveStopped
        | None ->
          match find_not_disequal d v with
          | `AllDiff al ->
            let pexp = Delayed.mk_pexp d expsubst (SubstUpFalse(the,al)) in
            Bool.set_false d pexp own; (** contradiction *)
            raise Impossible
          | `Found _ ->
            Demon.AliveStopped
      end
    | None ->
      match find_not_disequal d v with
      | `AllDiff al ->
        let pexp = Delayed.mk_pexp d expsubst (SubstUpFalse(the,al)) in
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
          Node.S.iter (Delayed.register d) v;
          let r = norm_dom d clsem in
          begin match r with
          | Demon.AliveReattached ->
            let events = Node.S.fold (fun cl acc ->
              (Demon.Create.EventChange(cl,()))::
                (Demon.Create.EventDom(cl,dom,()))::acc
              ) v [] in
            let events = Demon.Create.EventValue(own,Bool.dom,())::events in
            Demon.Key.attach d DaemonPropa.key v events;
            if true (* GenEquality.dodec (Th.get_ty v) *) then begin
              Debug.dprintf4 debug "[Equality] @[ask_dec on %a for %a@]"
                Node.pp own Th.pp v;
              Delayed.register_decision d (Trail.GCho(own,ChoEquals.key,clsem));
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
module ConDis = struct
  open Conflict

  type t = {
    l1 : Node.t;
    l0 : Node.t;
    r0 : Node.t;
    r1 : Node.t;
    disequality : Node.t;
    }

  let key : t Trail.Con.t = Trail.Con.create_key "Diff"

  let pp fmt c =
    Format.fprintf fmt "%a=%a≠%a=%a"
      Node.pp c.l1
      Node.pp c.l0
      Node.pp c.r0
      Node.pp c.r1

  let split t c cl1 cl2 =
    if Conflict.age_merge_opt t cl1 c.l1 = None then
      let cl1, cl2 = EqCon.orient_split t {l=c.r0;r=c.r1} cl1 cl2 in
      (Trail.Pcon.pcon key {c with r1 = cl1})::(EqCon.create_eq cl2 c.r1)
    else
      let cl1, cl2 = EqCon.orient_split t {l=c.l0;r=c.l1} cl1 cl2 in
      (Trail.Pcon.pcon key {c with l1 = cl1})::(EqCon.create_eq cl2 c.l1)


  let useful_nodes c =
    Bag.list [c.l1;c.l0;c.r1;c.r0]

  let levels t c =
    let l = Levels.empty in
    let l = Levels.add t (Conflict.age_merge t c.l1 c.l0) l in
    let l = Levels.add t (Conflict.age_merge t c.r1 c.r0) l in
    let l = Levels.add t (Conflict.age_merge t c.disequality Bool._false) l in
    l

  let apply_learnt c =
    equality [c.l1;c.r1], Neg

end

let () = Conflict.register_con(module ConDis)

module ExpMerge = struct
  open Conflict
  open D
  type t = expmerge

  let pp fmt = function
    | Merge  (pexp,cl1,cl2,i)   ->
      Format.fprintf fmt "Merge!(%a,%a,%a,%a)"
        pp_pexp pexp Node.pp cl1 Node.pp cl2 ThE.pp (Dis.to_node i)


  let analyse _ _ _ = raise Impossible

  let create_diff t cl1 cl2 i =
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
    let diff = Trail.Pcon.pcon ConDis.key {l1=cl1;l0=cl1_0;r0=cl2_0;r1=cl2;disequality=ThE.node the} in
    diff

  let from_contradiction t (Merge(pexp,cl1,cl2,i)) =
    let lcon = Conflict.analyse t pexp (Trail.Pcon.pcon EqCon.key {l=cl1;r=cl2}) in
    let diff = create_diff t cl1 cl2 i in
    diff::lcon

  let key = expmerge

end

let () = Conflict.register_exp(module ExpMerge)

module ExpSubst = struct
  open Conflict

  type t = expsubst

  let pp fmt = function
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

  let analyse t e pcon =
    match e with
    | SubstUpTrue    (v,e1,e2,_)   -> (** two are equals *)
      let own = ThE.node v in
      let lcon = Conflict.split t pcon own Bool._true in
      let pcon = Trail.Pcon.pcon EqCon.key {l=e1;r=e2} in
      pcon::lcon
    | SubstUpFalse   (v,al)   ->
      let own = ThE.node v in
      let lcon = Conflict.split t pcon own Bool._false in
      let al = CCList.diagonal al in
      let fold lcon ((e1,dis1),(e2,dis2)) =
        match dis1, dis2 with
        | Some dis1, Some dis2 ->
          let dis = Dis.inter dis1 dis2 in
          (** choose the oldest? *)
          let d = Dis.choose dis in
          let diff = ExpMerge.create_diff t e1 e2 d in
          diff::lcon
        | _ -> raise Impossible
      in
      List.fold_left fold lcon al
    | SubstDownTrue  (the)   -> begin
      let v = ThE.sem the in
      match Node.S.elements v with
      | [a;b] ->
        let lcon = Conflict.split t pcon a b in
        (EqCon.create_eq (ThE.node the) Bool._true)@lcon
      | _ -> raise Impossible
    end
    | SubstDownFalse (the,_)   ->
      let Trail.Pcon.Pcon(con,c) = pcon in
      let c = Con.Eq.coerce con ConDis.key c in
      let lcon = [] in
      let lcon = (EqCon.create_eq c.l1 c.l0)@lcon in
      let lcon = (EqCon.create_eq c.r1 c.r0)@lcon in
      let lcon = (EqCon.create_eq (ThE.node the) Bool._false)@lcon in
      lcon

  let key = expsubst

  let from_contradiction _ _ = raise Impossible

end


let () = Conflict.register_exp(module ExpSubst)


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

  let key = Sem.create_key "ite"

end

module EITE = Sem.Register(ITE)

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
    let pexp = Delayed.mk_pexp d expite (the,b) in
    Delayed.register d branch;
    Delayed.merge d pexp own branch

  let immediate = false
  let throttle = 100
  let wakeup d = function
    | Events.Fired.EventValue(cond,dom,clsem) ->
      assert (Value.equal dom Bool.dom);
      let v = EITE.sem clsem in
      assert (Delayed.is_equal d cond v.cond);
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
          Delayed.register d v.cond;
          Delayed.register d v.then_;
          Delayed.register d v.else_;
          Delayed.register_decision d (Trail.GCho(v.cond,Bool.chobool,v.cond));
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
    (* Trail.age -> *) t -> Trail.Pcon.t -> Trail.Pcon.t list =
    fun t (the,b) con ->
      let v = EITE.sem the in
      let own = EITE.node the in
      let lcon = Conflict.split t con own (if b then v.then_ else v.else_) in
      let pcon = EqCon.create_eq v.cond (if b then Bool._true else Bool._false) in
      pcon@lcon

  let from_contradiction _ _ = raise Impossible

end

let () = Conflict.register_exp(module ExpITE)


(** {2 Interpretations} *)
let () =
  let interp ~interp t =
    try
      let fold acc e = Values.S.add_new Exit (interp e) acc in
      let _ = Node.S.fold_left fold Values.S.empty t in
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
      | [a;b] when is equal_id || is equiv_id -> !< (Values.equal a b)
      | [c;a;b] when is ite_id -> Some (if (!> c) then a else b)
      | _   when is_distinct_id id -> begin
          try
            let fold acc v = Values.S.add_new Exit v acc in
            let _ = List.fold_left fold Values.S.empty args in
            Some Bool.values_false
          with Exit ->
            Some Bool.values_true
        end
      | _ -> None
    )


let converter d f l =
  let of_term t =
    let n = SynTerm.node_of_term t in
    Egraph.Delayed.register d n;
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
  ()
