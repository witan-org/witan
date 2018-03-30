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
open Popop
open Stdlib
open Types
open Solver

let debug = Debug.register_info_flag
  ~desc:"for the equality and disequality predicate"
  "disequality"

module Dis = struct
  include DInt.S
  let pp fmt s =
    Format.fprintf fmt "{%a}"
      (Pp.iter1 DInt.S.iter Pp.semi Stdlib.DInt.pp) s
end

let dom : Dis.t dom = Dom.create_key "dis"

module D = struct
  type t = Dis.t

  let merged (b1:t option) (b2 :t option) =
    match b1,b2 with
    | Some b1, Some b2 -> b1 == b2 (** not Dis.equality *)
    | None, None -> true
    | _ -> false

  type expmerge =
  | Merge of Explanation.pexp * Cl.t * Cl.t * int

  let expmerge : expmerge Explanation.exp =
    Explanation.Exp.create_key "Equality.merge"

  let merge d pexp (s1,cl1) (s2,cl2) _ =
    match s1, s2 with
    | None, None -> raise Impossible
    | Some s, None ->
      Delayed.set_dom_premerge d dom cl2 s
    | None, Some s ->
      Delayed.set_dom_premerge d dom cl1 s
    | Some s1, Some s2 ->
      let s = DInt.M.union (fun i () ->
          let pexp = Delayed.mk_pexp d expmerge (Merge(pexp,cl1,cl2,i)) in
          Delayed.contradiction d pexp) s1 s2 in
      Delayed.set_dom_premerge d dom cl1 s;
      Delayed.set_dom_premerge d dom cl2 s


  let pp fmt s = Dis.pp fmt s
  let key = dom
end

module DE = RegisterDom(D)

let set_dom d pexp cl s =
  let s = match Delayed.get_dom d dom cl with
    | Some s' ->
      DInt.M.union (fun _i () -> assert false) s' s
    | None -> s in
  Delayed.set_dom d pexp dom cl s


type t = Cl.S.t


let sem : t sem = Sem.create_key "Eq"

module Th = struct

  let get_ty v = Cl.ty (fst (Cl.M.choose v))

  let inv s = not (Cl.M.is_empty s || Cl.M.is_num_elt 1 s) &&
              let ty = get_ty s in
              (Cl.M.for_all (fun e _ -> Ty.equal ty (Cl.ty e)) s)

  let only_two s =
    assert (inv s);
    if Cl.M.is_num_elt 2 s then
      let enum = Cl.M.start_enum s in
      let (cl1,()), enum = Opt.get (Cl.M.val_enum enum), Cl.M.next_enum enum in
      let (cl2,())       = Opt.get (Cl.M.val_enum enum) in
      Some (cl1,cl2)
    else None


  module T : OrderedHashedType with type t = Cl.S.t = struct
    include Cl.S

    let hash s = Cl.S.fold (fun e acc -> Hashcons.combine acc (Cl.hash e)) s 29

    let pp fmt s =
      assert (inv s);
      match only_two s with
      | Some (cl1,cl2) ->
        Format.fprintf fmt "%a=@,%a" Cl.pp cl1 Cl.pp cl2
      | None ->
        Format.fprintf fmt "or=(%a)"
          (Pp.iter1 Cl.S.iter Pp.comma Cl.pp) s
  end

  include T
  include MkDatatype(T)

  let key = sem



  (* let propagate ~propagate s = Cl.S.iter propagate s *)

end

module ThE = RegisterSem(Th)

(**)
type special_equality = {
  equality:
    Conflict.ComputeConflict.t -> Cl.t -> Cl.t -> unit;
  disequality:
    Conflict.ComputeConflict.t -> Explanation.Age.t ->
    hyp:bool -> Cl.t -> Cl.t -> Cl.t -> Cl.t -> unit;
  merged:
    Conflict.ComputeConflict.t ->
    Explanation.Deps.t -> Explanation.Age.t ->
    Cl.t -> Cl.t ->
    Explanation.pexp ->
    Cl.t -> Cl.t -> Explanation.Deps.t;
  dodec: bool;
  new_true_disequality: Solver.Delayed.t -> Cl.t -> Cl.S.t -> unit
}

let expspecial_of_sort = Ty.H.create 20

let register_sort ty spe =
  Ty.H.add expspecial_of_sort ty spe
(**)

let check_sem v cl =
  let own = ThE.cl (ThE.index v Bool.ty) in
  Cl.equal cl own

(** API *)

let equality cll =
  try
    let fold acc e = Cl.S.add_new Exit e acc in
    let s = List.fold_left fold Cl.S.empty cll in
    Cl.index sem s Bool.ty
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

let new_tag =
  let c = ref (-1) in
  fun () -> incr c;
    let c = !c in
    c, fun () -> Dis.singleton c (** each instance of this tag must not be == *)

exception Found of Cl.t * Cl.t

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
    let s = Cl.M.fold_left (fun acc cl () ->
        (cl,Delayed.get_dom d dom cl)::acc) [] s in
    outer_loop s;
    None
  with Found (cl1,cl2) ->
    Some (cl1,cl2)

type expsubst =
| SubstUpTrue of t * Cl.t (* e1 *) * Cl.t (* e2 *) * Cl.t
| SubstUpFalse of t * Cl.t
| SubstDownTrue of t * Cl.t
| SubstDownFalse of t * Cl.t * int
| SubstDownDec of Explanation.pexp * int
| Cst

let expsubst : expsubst Explanation.exp =
  Explanation.Exp.create_key "Equality.subst"

let cst_i, cst_tag =
  let i,stag = new_tag () in
  i, (fun env ->
      let pexp = Delayed.mk_pexp env expsubst Cst in
      pexp, stag)

let norm_set d own v =
  try
    ignore (Cl.S.fold_left (fun acc e0 ->
        let e = Delayed.find_def d e0 in
        Cl.M.add_change (fun _ -> e0)
            (fun e0 e0' -> raise (Found(e0',e0)))
            e e0 acc)
        Cl.M.empty v);
    false
  with Found (e1,e2) ->
    (** TODO remove that and choose what to do. ex: int real *)
    let pexp = Delayed.mk_pexp d expsubst (SubstUpTrue (v,e1,e2,own)) in
    Bool.set_true d pexp own;
    true

(** Conflict *)
type eqconflict =
  | Eq : Cl.t * Cl.t * bool -> eqconflict

module EqConflict = struct


  module T = struct

      type t = eqconflict

      let print_equal fmt b =
        if b
        then Format.pp_print_string fmt "=="
        else Format.pp_print_string fmt "!="

      let pp fmt = function
        | Eq(cl1,cl2,b) ->
          Format.fprintf fmt "%a%a%a" Cl.pp cl1 print_equal b Cl.pp cl2

      let equal e1 e2 =
        match e1, e2 with
        | Eq(cla1,clb1,b1), Eq(cla2,clb2,b2) ->
          Cl.equal cla1 cla2 && Cl.equal clb1 clb2 && DBool.equal b1 b2

      let compare e1 e2 =
        match e1, e2 with
        | Eq(cla1,clb1,b1), Eq(cla2,clb2,b2) ->
          let c = Cl.compare cla1 cla2 in
          if c != 0 then c else
            let c = Cl.compare clb1 clb2 in
            if c != 0 then c else
              DBool.compare b1 b2

      let hash e1 =
        match e1 with
        | Eq(cla1,clb1,b) ->
          Hashcons.combine2 (Cl.hash cla1) (Cl.hash clb1) (DBool.hash b)
  end

  include T
  include Stdlib.MkDatatype(T)

  let mk_eq cl1 cl2 b =
    if Cl.compare cl1 cl2 <= 0
    then Eq(cl1,cl2,b)
    else Eq(cl2,cl1,b)

end

let choequal : (EqConflict.t,unit) Explanation.cho =
  Explanation.Cho.create_key "Equal.cho"

(** Default equality/disequality conflict *)
module ConDefaultEq = struct
  open Conflict
  open EqConflict

  type t = EqConflict.t Bag.t

  let pp fmt b =
    Bag.pp Pp.semi EqConflict.pp fmt b

  let key = Explanation.Con.create_key "Equality.gen"

  let print_eqs fmt eqs =
      Pp.iter1 EqConflict.S.iter Pp.semi EqConflict.pp fmt eqs

  class finalized eqs : Conflict.finalized = object
    method pp fmt = print_eqs fmt eqs
    method test d =
      let fold acc v =
        let f cl1 cl2 b =
          let return b =
            if b then raise Exit (** true *) else acc (** false *) in
          (** the negation of the constraint is evaluated *)
          if Delayed.is_equal d cl1 cl2 then return (not b)
          else if is_disequal d cl1 cl2
          then return b
          else ToDecide
        in
        match v with
        | Eq(cl1,cl2,b) ->
          f cl1 cl2 b
      in
      try
        EqConflict.S.fold_left fold False eqs
      with Exit -> True
    method decide :
      'a. 'a Conflict.fold_decisions -> Solver.Delayed.t -> 'a -> 'a =
      fun f d acc ->
      let fold acc v =
        let return cl1 cl2 c =
          if Delayed.is_equal d cl1 cl2 || is_disequal d cl1 cl2 then acc
          else f.fold_decisions acc choequal c () in
        match v with
        | Eq(cl1,cl2,b) ->
          return cl1 cl2 (Eq(cl1,cl2,not b))
      in
      EqConflict.S.fold_left fold acc eqs
    method conflict_add _ =
      let fold acc v =
        let f cl1 cl2 b =
          let eq = equality [cl1;cl2] in
          Cl.M.add eq b acc in
        (** no find because the explication would be different *)
        match v with
        | Eq(cl1,cl2,b) ->
          f cl1 cl2 b
      in
      EqConflict.S.fold_left fold Cl.M.empty eqs
  end

  let finalize _ l =
    let m =
      Bag.fold_left (fun acc b ->
          Bag.fold_left (fun acc e -> EqConflict.S.add e acc) acc b)
        EqConflict.S.empty l in
    Debug.dprintf2 Conflict.print_conflicts "[Equality] @[conflict: %a@]"
      print_eqs m;
    if EqConflict.S.is_empty m then None
    else Some (new finalized m)

  let get_con = Conflict.fold_requested (fun b1 _ b2 -> Bag.concat b1 b2)

  let clatlimit t age cl rcl =
    if ComputeConflict.before_first_dec t age
    then GRequested Bag.empty
    else
      let v = mk_eq cl rcl true in
      ComputeConflict.set_dec_cho t choequal v;
      GRequested(Bag.elt v)


  let eq_sym s = s
  let eq_transitivity s1 s2 = Bag.concat s1 s2
  let eq_check ~from:_ ~to_:_ _ = true
  let eq_other ~from:_ ~to_:_ = Bag.empty
  let finish _ x = x
end

module EConDefaultEq = Conflict.RegisterCon(ConDefaultEq)

let () =
  Bool.mk_conequal := (fun t cl1 cl2 ->
      let v = EqConflict.mk_eq cl1 cl2 true in
      Conflict.ComputeConflict.set_dec_cho t choequal v;
      Conflict.GOther(ConDefaultEq.key,Bag.elt v))

module ChoEquals = struct
  open Conflict

  module Key = Th
  module Data = struct
    type t = Cl.t * Cl.t
    let pp fmt (cl1,cl2) =
      Format.fprintf fmt "(%a = %a)"
        Cl.pp cl1 Cl.pp cl2
  end

  let key = Explanation.Cho.create_key "Equals.cho"

  let choose_decision d v =
    let own = Cl.index sem v Bool.ty in
      Debug.dprintf4 debug "[Equality] @[dec on %a for %a@]"
        Cl.pp own Th.pp v;
      if norm_set d own v
      then DecNo
      else
        match find_not_disequal d v with
        | None ->
          let pexp = Delayed.mk_pexp d expsubst (SubstUpFalse(v,own)) in
          Bool.set_false d pexp own;
          DecNo
        | Some (cl1,cl2) ->
          DecTodo(cl1,cl2)

  let make_decision d dec v (cl1,cl2) =
    Debug.dprintf6 Conflict.print_decision
      "[Equality] @[decide on merge of %a and %a in %a@]"
      Cl.pp cl1 Cl.pp cl2 Th.pp v;
    let pexp = Explanation.mk_pcho dec key v (cl1,cl2) in
    Delayed.register d cl1;
    Delayed.register d cl2;
    Delayed.merge d pexp cl1 cl2


  let analyse (type a) t (con: a Explanation.con) v (cl1,cl2) =
    ComputeConflict.set_dec_cho t key v;
    return con ConDefaultEq.key (Bag.elt (EqConflict.mk_eq cl1 cl2 true))

end

module EChoEquals = Conflict.RegisterCho(ChoEquals)

module ChoEqual = struct
  open Conflict

  module Key = EqConflict
  module Data = DUnit

  let key = choequal

  let choose_decision d v =
    let f cl1 cl2 =
      if Delayed.is_equal d cl1 cl2 || is_disequal d cl1 cl2 then DecNo
      else DecTodo ()
    in
    match v with
    | Eq(cl1,cl2,_) ->
      f cl1 cl2

  let make_decision d dec v () =
    Debug.dprintf2 Conflict.print_decision
      "[Equality] @[decide on %a@]" EqConflict.pp v;
    let pexp = Explanation.mk_pcho dec key v () in
    let cl1,cl2,b (* equality *) =
    match v with
    | Eq(cl1,cl2,b) ->
      cl1,cl2, b
    in
    if b then begin
      Delayed.register d cl1;
      Delayed.register d cl2;
      Delayed.merge d pexp cl1 cl2
    end else begin
      let dis, stag = new_tag () in
      let pexp = Delayed.mk_pexp d expsubst (SubstDownDec(pexp,dis)) in
      let set_dom cl =
        Delayed.register d cl;
        set_dom d pexp cl (stag ()) in
      assert (not (Delayed.is_equal d cl1 cl2));
      set_dom cl1; set_dom cl2
    end

  let analyse (type a) t (con: a Explanation.con) v () =
    ComputeConflict.set_dec_cho t choequal v;
    return con ConDefaultEq.key (Bag.elt v)

end

module EChoEqual = Conflict.RegisterCho(ChoEqual)

let norm_dom d own v =
  if norm_set d own v
  then Demon.AliveStopped
  else begin
    Debug.dprintf4 debug "[Equality] @[norm_dom %a %a@]"
      Cl.pp own Th.pp v;
    match Bool.is d own with
    | Some false ->
      let dis, stag = new_tag () in
      let pexp =
        Delayed.mk_pexp d expsubst (SubstDownFalse(v,own,dis)) in
      Cl.S.iter (fun cl -> set_dom d pexp cl (stag ())) v;
      Opt.iter
        (fun f -> f.new_true_disequality d own v)
        (Ty.H.find_opt expspecial_of_sort (Cl.ty own));
      Demon.AliveStopped
    | Some true ->
      begin match Th.only_two v with
        | Some (cl1,cl2) ->
          let pexp = Delayed.mk_pexp d expsubst (SubstDownTrue(v,own)) in
          Delayed.merge d pexp cl1 cl2; Demon.AliveStopped
        | None ->
          match find_not_disequal d v with
          | None ->
            let pexp = Delayed.mk_pexp d expsubst
                (SubstUpFalse(v,own)) in
            Bool.true_is_false d own pexp
          | Some _ ->
            Demon.AliveStopped
      end
    | None ->
      match find_not_disequal d v with
      | None ->
        let pexp = Delayed.mk_pexp d expsubst (SubstUpFalse(v,own)) in
        Bool.set_false d pexp own;
        Demon.AliveStopped
      | Some _ -> (** they are still not proved disequal *)
        Demon.AliveReattached
  end

(** Register equality/disequality exp for types *)

module GenEquality = struct
  open Explanation
  open Conflict

  let equality t cl1 cl2 =
    (* Format.fprintf (Debug.get_debug_formatter ()) *)
    (*   "[equality] @[%a at %a@]" *)
    (*   Conflict.print_rlist rlist Age.pp age; *)
    let b,deps =
      ComputeConflict.Equal.one_equal t ~from:cl1 ~to_:cl2
        ConDefaultEq.key Bag.empty Deps.empty in
    ComputeConflict.add_deps t deps;
    ComputeConflict.unknown_con t ConDefaultEq.key b

  let expspecial ~dodec =
    { equality;
      disequality = (fun t _age ~hyp:_ cl1d cl1e cl2e cl2d ->
          equality t cl1d cl1e;
          equality t cl2e cl2d);
      merged = (fun t deps _age cl1d cl1 pexp cl2 cl2d ->
          let con = ConDefaultEq.key in
          let b = Bag.empty in
          let eq_t = ComputeConflict.Equal.init con b deps ~from:cl1d in
          let eq_t = ComputeConflict.Equal.add_equal t eq_t ~to_:cl1 in
          let eq_t = ComputeConflict.Equal.add_pexp t eq_t ~to_:cl2 pexp in
          let eq_t = ComputeConflict.Equal.add_equal t eq_t ~to_:cl2d in
          let b,deps = ComputeConflict.Equal.close eq_t in
          Explanation.Deps.add_unknown_con deps ConDefaultEq.key b);
      dodec;
      new_true_disequality = (fun _ _ _ -> ());
    }

  let def_expspecial = expspecial ~dodec:true

  let find_expspecial ty =
      Ty.H.find_def expspecial_of_sort def_expspecial ty

  let equality t r1 r2 ty =
    (find_expspecial ty).equality t r1 r2

  let disequality t age r1d r1e r2e r2d ty =
    (find_expspecial ty).disequality t age r1d r1e r2e r2d

  let merged t deps age pexp cl1 rl1 cl2 rl2 ty =
    let deps0 = ComputeConflict.get_current_deps t in
    let deps = (find_expspecial ty).merged t deps age pexp cl1 rl1 cl2 rl2 in
    (** No dependencies should have been added, all of them must be in deps *)
    assert (deps0 == ComputeConflict.get_current_deps t);
    deps
(*
  let repr t deps age cl1 rl1 ty =
    let deps0 = ComputeConflict.get_current_deps t in
    let deps = (find_expspecial ty).repr t deps age cl1 rl1 in
    assert (deps0 == ComputeConflict.get_current_deps t);
    deps
*)

  let dodec ty = (find_expspecial ty).dodec

  let new_true_disequality ty = (find_expspecial ty).new_true_disequality
end

let register_sort_con ty ~dodec con = register_sort ty
    (GenEquality.expspecial ~dodec)

let () = register_sort_con Bool.ty ~dodec:false Conflict.conclause

(** Propagation *)

module DaemonPropa = struct
  type k = Th.t
  type d = unit
  let key = Demon.Key.create "Equality.DaemonPropa"

  module Key = Th
  module Data = DUnit
  type info = unit let default = ()

  let immediate = false
  let wakeup d v _ev () =
    norm_dom d (Cl.index sem v Bool.ty) v

end

module RDaemonPropa = Demon.Key.Register(DaemonPropa)

module DaemonInit = struct
  type k = unit
  type d = unit
  let key = Demon.Key.create "Equality.DaemonInit"

  module Key = DUnit
  module Data = DUnit
  type info = unit let default = ()

  let immediate = true
  let wakeup d () ev () =
    List.iter
      (function Events.Fired.EventRegSem(clsem,()) ->
        begin
          let clsem = ThE.coerce_clsem clsem in
          let v = ThE.sem clsem in
          let own = ThE.cl clsem in
          Cl.S.iter (Delayed.register d) v;
          let r = norm_dom d own v in
          begin match r with
          | Demon.AliveReattached ->
            let events = Cl.S.fold (fun cl acc ->
              (Demon.Create.EventChange(cl,()))::
                (Demon.Create.EventDom(cl,dom,()))::acc
              ) v [] in
            let events = Demon.Create.EventDom(own,Bool.dom,())::events in
            Demon.Key.attach d DaemonPropa.key v events;
            if GenEquality.dodec (Th.get_ty v) then begin
              Debug.dprintf4 debug "[Equality] @[ask_dec on %a for %a@]"
                Cl.pp own Th.pp v;
              Delayed.register_decision d (Explanation.GCho(ChoEquals.key,v));
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

  type e =
    | Necessary of Cl.t
    | Hideable of Cl.t
    | Unknown

  type t = (e * Explanation.Deps.t) DInt.M.t

  let key : t Explanation.con = Explanation.Con.create_key "Diff"

  let print_e fmt (c,_) =
    match c with
    | Necessary cl -> Format.fprintf fmt "%a,nece" Cl.pp cl
    | Hideable cl -> Format.fprintf fmt "%a,hide" Cl.pp cl
    | Unknown -> Format.fprintf fmt "unk"

  let pp fmt m =
    Format.fprintf fmt "{%a}"
      (Pp.iter2 DInt.M.iter Pp.semi Pp.colon DInt.pp
         print_e) m

  (** Use only on dom domain *)

  let same_sem _ _ _ _ _ _ _ = raise Impossible (** never used on that *)
  let finalize _ _  = raise Impossible (** never used on that *)

  let clatlimit _ _ _ = raise Impossible

  (** not used for equality *)
  let eq_sym _ = raise Impossible
  let eq_transitivity _ _ = raise Impossible
  let eq_check ~from:_ ~to_:_ _ = raise Impossible
  let eq_other ~from:_ ~to_:_ = raise Impossible

  let finish _ x = x

end

module EConDis = Conflict.RegisterCon(ConDis)

module ExpMerge = struct
  open Explanation
  open Conflict
  open D
  type t = expmerge

  let pp fmt = function
    | Merge  (pexp,cl1,cl2,i)   ->
      Format.fprintf fmt "Merge!(%a,%a,%a,%i)"
        Explanation.pp_pexp pexp Cl.pp cl1 Cl.pp cl2 i


(*
  let iterexp t age = function
    | Merge    ((pexp,cl1,cl2), repr_cl)    ->
      IterExp.need_pexp t pexp;
      IterExp.need_dom t age cl1 dom;
      IterExp.need_cl_repr t age cl2;
      Opt.iter (fun cl -> IterExp.need_dom t age cl dom) repr_cl
    | DomMerge ((pexp,cl),repr_cl)    ->
      IterExp.need_pexp t pexp;
      Opt.iter (fun cl -> IterExp.need_dom t age cl dom) repr_cl;
      IterExp.need_cl_repr t age cl
*)

  module Deps = Explanation.Deps

  let get_dom_dis t deps pexp =
    let rc, deps' = ComputeConflict.get_pexp_deps t pexp ConDis.key in
    let deps = Deps.concat deps deps' in
    match rc with
    | GRequested s when Deps.is_empty deps -> s
    | GRequested s ->
      DInt.M.map (fun (cl,deps') -> cl,Deps.concat deps' deps) s
    (** equality is the only one to play with dom except fact *)
    | GOther (con,_) ->
      (* Format.fprintf (Debug.get_debug_formatter ()) *)
      (*   "[Equality] @[Impossible %a@]" Explanation.Con.pp con; *)
      raise Impossible


  let mk_return_eq t cl1 cl2 =
    let open ConDis in
    match cl1,cl2 with
    (** The conflict comes from a propagation bool -> equality *)
    | Unknown, Unknown |
      (** The conflict comes from a explimit dom *)
      Hideable _, Hideable _ -> true
    | Necessary cl1, Necessary cl2 ->
      let v = EqConflict.mk_eq cl1 cl2 false in
      ComputeConflict.set_dec_cho t ChoEqual.key v;
      ComputeConflict.unknown_con t ConDefaultEq.key (Bag.elt v);
      false
    (** It should come from the same propagation *)
    | _,_ -> raise Impossible

  let analyse :
  type a. Conflict.ComputeConflict.t ->
    Explanation.age -> a Explanation.con -> t -> a Conflict.rescon =
    fun t age con exp ->
      (** this union unify if its discons but create a disequality
          in the other case *)
      match exp with
      | Merge (pexp,cl1,cl2,i)    ->
        let ty = Cl.ty cl1 in
        let mod_dom1 = ComputeConflict.get_dom t age cl1 dom in
        let mod_dom2 = ComputeConflict.get_dom t age cl2 dom in
        let find_mod l =
          let search mod_dom =
            let m = get_dom_dis t Deps.empty mod_dom.Explanation.modpexp in
            match DInt.M.find_opt i m with
            | None -> None
            | Some (cl,deps) -> Some (mod_dom,cl,deps) in
          try Lists.first search l
          with Not_found -> raise Impossible
        in
        Debug.dprintf4 debug "mod_dom1:%a , mod_dom2:%a"
          (Pp.list Pp.semi Explanation.print_mod_dom) mod_dom1
          (Pp.list Pp.semi Explanation.print_mod_dom) mod_dom2;
        let (mod_dom1,cld1,deps1) = find_mod mod_dom1 in
        let (mod_dom2,cld2,deps2) = find_mod mod_dom2 in
        let deps3 = GenEquality.merged t Deps.empty age
            mod_dom1.modcl cl1 pexp cl2 mod_dom2.modcl ty in
        ComputeConflict.add_deps t
          (Explanation.Deps.concatl [deps1;deps2;deps3]);
        (** It is a contradiction *)
        ignore (mk_return_eq t cld1 cld2);
        return con conclause Cl.M.empty

  let expdomlimit _ _ _ _ _ _ _ = raise Impossible
      (** used only for unsat *)

  let key = expmerge

  let same_sem t age _sem _v con exp _cl1 _cl2 =
    analyse  t age con exp

end

module EExpMerge = Conflict.RegisterExp(ExpMerge)

module ExpSubst = struct
  open Explanation
  open Conflict
  open ComputeConflict

  type t = expsubst

  let pp fmt = function
    | SubstUpTrue    (v,e1,e2,cl)   -> (** two are equals *)
      Format.fprintf fmt "SubstUpTrue(%a,%a,%a,%a)"
        Th.pp v Cl.pp e1 Cl.pp e2 Cl.pp cl
    | SubstUpFalse   (v,cl)   ->
      Format.fprintf fmt "SubstUpFalse(%a,%a)"
        Th.pp v Cl.pp cl
    | SubstDownTrue  (v,own)   ->
      Format.fprintf fmt "SubstDownTrue(%a,%a)"
        Th.pp v Cl.pp own
    | SubstDownFalse (v,own,i)   ->
      Format.fprintf fmt "SubstDownFalse(%a,%a,%i)"
        Th.pp v Cl.pp own i
    | SubstDownDec (pexp,i)   ->
      Format.fprintf fmt "SubstDownDec(%a,%i)"
        Explanation.pp_pexp pexp i
    | Cst ->
      Format.fprintf fmt "Cst"
(*
  let iterexp t age = function
    | SubstUpTrue    (v,e1,e2,_)   -> (** two are equals *)
      need_sem t age sem v;
      need_cl_repr t age e1;
      need_cl_repr t age e2
    | SubstUpFalse   (v,_)   ->
      need_sem t age sem v;
      Cl.S.iter (fun cl -> need_dom t age cl dom) v
    | SubstDownTrue  (v,l,own)   ->
      need_sem t age sem v;
      List.iter (fun cl -> need_cl_repr t age cl) l;
      need_dom t age own Bool.dom
    | SubstDownFalse (v,l,own,_)   ->
      need_sem t age sem v;
      List.iter (fun cl -> need_cl_repr t age cl) l;
      need_dom t age own Bool.dom
    | SubstDownDec (pexp,_)   ->
      need_pexp t pexp
    | Cst -> ()
*)


  let return_diseq t age con cl1 s1 cl2 s2 ty clauses =
    let open ConDis in
    let s =
      DInt.M.inter (fun _ (moddom1,cld1,deps1) (moddom2,cld2,deps2) ->
          Some (moddom1,moddom2,cld1,cld2,Deps.concat deps1 deps2)) s1 s2 in
    assert (not (DInt.M.is_empty s));
    let _,(moddom1,moddom2,cld1,cld2,deps) = DInt.M.choose s in
    ComputeConflict.add_deps t deps;
    let hyp = ExpMerge.mk_return_eq t cld1 cld2 in
    GenEquality.disequality t age ~hyp
      moddom1.modcl cl1
      cl2 moddom2.modcl ty;
    return con conclause clauses


  let analyse :
  type a. Conflict.ComputeConflict.t ->
    Explanation.age -> a Explanation.con -> t -> a Conflict.rescon =
    fun t age con exp ->
      let return_dis (con : a con) i : a Conflict.rescon =
        match Explanation.Con.Eq.coerce_type con ConDis.key with
        | Types.Eq ->
          GRequested (DInt.M.singleton i
                        (ConDis.Unknown, Explanation.Deps.empty))
      in
    match exp with
    | SubstUpTrue    (v,e1,e2,_)   -> (** two are equals *)
      let ty = Th.get_ty v in
      let s = Cl.M.empty in
      GenEquality.equality t e1 e2 ty;
      return con conclause s
    | SubstUpFalse   (v,_)   ->
      let ty = Th.get_ty v in
      let s = Cl.M.empty in
      let l = Cl.S.fold_left
          (fun acc cl -> (cl,ComputeConflict.get_dom t age cl dom)::acc) [] v in
      begin match l with
        | []| [_] -> assert false
        | [cl1,moddom1;cl2,moddom2] ->
          Debug.dprintf4 debug "moddom1:%a , moddom2:%a"
            (Pp.list Pp.semi Explanation.print_mod_dom) moddom1
            (Pp.list Pp.semi Explanation.print_mod_dom) moddom2;
          let get_dom_disl l =
            List.fold_left (fun acc moddom ->
                let s = ExpMerge.get_dom_dis t Deps.empty moddom.modpexp in
                let s = DInt.M.map (fun (cl,deps) -> (moddom,cl,deps)) s in
                let s = DInt.M.union (fun _ _ -> raise Impossible) acc s in
                s
              ) DInt.M.empty l in
          let dis1 = get_dom_disl moddom1 in
          let dis2 = get_dom_disl moddom2 in
          return_diseq t age con cl1 dis1 cl2 dis2 ty s
        | _ -> assert false (* TODO *)
      end
    | SubstDownTrue  (v,own)   ->
      let ty = Th.get_ty v in
      assert (check_sem v own);
      let s = Bool.get_dom t age own Cl.M.empty in
      return con conclause s
    | SubstDownFalse (v,own,i)   ->
      let ty = Th.get_ty v in
      assert (check_sem v own);
      let s = Bool.get_dom t age own Cl.M.empty in
      unknown_con t conclause s;
      return_dis con i
    | SubstDownDec (pexp,i)   ->
      begin match get_pexp t pexp ConDefaultEq.key with
        | GRequested v -> unknown_con t ConDefaultEq.key v
        | GOther _ -> raise Impossible
      end;
      return_dis con i
    | Cst ->
      return_dis con cst_i

  let expdomlimit :
  type a b. Conflict.ComputeConflict.t ->
    Explanation.age -> b dom -> Cl.t ->
    a Explanation.con -> b option -> t -> a Conflict.rescon =
    fun t age dom' cl con _ exp ->
      let return_dis (con : a con) i : a Conflict.rescon =
        return con ConDis.key
          (DInt.M.singleton i
             ((if ComputeConflict.before_first_dec t age
               then ConDis.Hideable cl
               else ConDis.Necessary cl), Explanation.Deps.empty))
      in
    match exp with
    | SubstUpTrue    (v,e1,e2,_)   -> (** two are equals *)
      assert (Dom.equal dom' Bool.dom);
      let s = Cl.M.singleton cl true in
      return con conclause s
    | SubstUpFalse   (v,_)   ->
      assert (Dom.equal dom' Bool.dom);
      let s = Cl.M.singleton cl false in
      return con conclause s
    | SubstDownTrue  (v,own)   ->
      raise Impossible (** propagate a merge *)
    | SubstDownFalse (_,_,i)   ->
      assert (Dom.equal dom' dom);
      return_dis con i
    | SubstDownDec (pexp,i)   ->
      return_dis con i
    | Cst ->
      return_dis con cst_i

  let key = expsubst

  let same_sem t age _sem _v con exp _cl1 _cl2 =
    analyse  t age con exp

end

module EExpSubst = Conflict.RegisterExp(ExpSubst)


(** ITE *)
type ite = {cond: Cl.t; then_: Cl.t; else_: Cl.t}

module ITE = struct

  module TITE = struct
    type t = ite
    let equal x y = Cl.equal x.cond y.cond &&
                    Cl.equal x.then_ y.then_ &&
                    Cl.equal x.else_ y.else_
    let compare x y =
      let c = Cl.compare x.cond y.cond in
      if c != 0 then c
      else let c = Cl.compare x.then_ y.then_ in
        if c != 0 then c
        else Cl.compare x.else_ y.else_
    let hash x =
      Hashcons.combine2 (Cl.hash x.cond) (Cl.hash x.then_) (Cl.hash x.else_)

    let pp fmt x =
      Format.fprintf fmt "ite(%a,%a,%a)"
        Cl.pp x.cond Cl.pp x.then_ Cl.pp x.else_
  end

  include TITE
  include MkDatatype(TITE)

  let key = Sem.create_key "ite"

end
open ITE

module EITE = Types.RegisterSem(ITE)

let ite cond then_ else_ =
  let ty1 = Cl.ty then_ in
  let ty2 = Cl.ty else_ in
  assert (Ty.equal ty1 ty2);
  Cl.index ITE.key { cond; then_; else_} ty1

let expite : (ITE.t * bool) Explanation.exp =
  Explanation.Exp.create_key "Ite.exp"

module DaemonPropaITE = struct
  type d = EITE.t
  let key = Demon.Fast.create "ITE.propa"

  module Data = EITE

  let simplify d own b v =
    let branch = if b then v.then_ else v.else_ in
    let pexp = Delayed.mk_pexp d expite (v,b) in
    Delayed.register d branch;
    Delayed.merge d pexp own branch

  let immediate = false
  let throttle = 100
  let wakeup d = function
    | Events.Fired.EventDom(cond,dom,clsem) ->
      assert (Dom.equal dom Bool.dom);
      let v = EITE.sem clsem in
      assert (Delayed.is_equal d cond v.cond);
      let own = EITE.cl clsem in
      begin match Bool.is d v.cond with
        | None -> assert false
        | Some b -> simplify d own b v
      end
    | _ -> raise UnwaitedEvent

end

module RDaemonPropaITE = Demon.Fast.Register(DaemonPropaITE)

module DaemonInitITE = struct
  type d = unit
  let key = Demon.Fast.create "ITE.init"

  module Key = DUnit
  module Data = DUnit

  let immediate = false
  let throttle = 100
  let wakeup d = function
    | Events.Fired.EventRegSem(clsem,()) ->
      begin
        let clsem = EITE.coerce_clsem clsem in
        let v = EITE.sem clsem in
        let own = EITE.cl clsem in
        match Bool.is d v.cond with
        | Some b ->
          DaemonPropaITE.simplify d own b v
        | None ->
          let clsem = EITE.index v (Cl.ty own) in
          assert (Cl.equal (EITE.cl clsem) own);
          Delayed.register d v.cond;
          Delayed.register d v.then_;
          Delayed.register d v.else_;
          Delayed.register_decision d (Explanation.GCho(Bool.chobool,v.cond));
          let events = [Demon.Create.EventDom(v.cond,Bool.dom,clsem)] in
          Demon.Fast.attach d DaemonPropaITE.key events
    end
    | _ -> raise UnwaitedEvent

end

module RDaemonInitITE = Demon.Fast.Register(DaemonInitITE)

module ExpITE = struct
  open Conflict

  type t = ITE.t * bool
  let key = expite

  let pp fmt (ite,b) =
    Format.fprintf fmt "(%a,%b)" ITE.pp ite b
(*
  let iterexp t age (ite,_) =
    IterExp.need_sem t age ITE.key ite;
    IterExp.need_dom t age ite.cond Bool.dom
*)
  let analyse :
      type a. Conflict.ComputeConflict.t ->
    Explanation.age -> a Explanation.con -> t -> a Conflict.rescon =
    fun t age con (ite,_) ->
    let s = Cl.M.empty in
    let s = Bool.get_dom t age ite.cond s in
    return con conclause s

  let expdomlimit _ _ _ _ _ _ _ = raise Impossible (** merge *)

  let same_sem t age _sem _v con exp _cl1 _cl2 =
    analyse  t age con exp

end

module EExpITE = Conflict.RegisterExp(ExpITE)

let th_register env =
  RDaemonPropa.init env;
  RDaemonInit.init env;
  RDaemonPropaITE.init env;
  RDaemonInitITE.init env;
  Demon.Key.attach env
    DaemonInit.key () [Demon.Create.EventRegSem(sem,())];
  Demon.Fast.attach env
    DaemonInitITE.key [Demon.Create.EventRegSem(ITE.key,())];
  let pexp,stag = cst_tag env in
  Delayed.set_dom env pexp dom Bool._true (stag ());
  Delayed.set_dom env pexp dom Bool._false (stag ())
