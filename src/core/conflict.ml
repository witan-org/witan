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

open Witan_core_structures

let print_decision = Debug.register_info_flag
  ~desc:"for@ the@ printing@ of@ the@ decisions@ done."
  "decisions"

let debug = Debug.register_info_flag
  ~desc:"for@ the@ main@ part@ of@ conflicts."
  "Conflict.core"

module Levels = struct
  type t =
    | No
    | One of Trail.Age.t
    | Two of Trail.Age.t * Trail.Age.t
  [@@ deriving eq]

  let pp fmt = function
    | No -> Pp.string fmt "-"
    | One age -> Trail.Age.pp fmt age
    | Two (age1,age2) -> Format.fprintf fmt "%a,%a" Trail.Age.pp age1 Trail.Age.pp age2

  let compare l1 l2 =
    match l1, l2 with
    | No, No -> 0
    | _, No -> 1
    | No, _ -> -1
    | One a, One b -> Trail.Age.compare a b
    | One a, Two (b,_) ->
      let c = Trail.Age.compare a b in
      if c <> 0 then c else -1
    | Two (a,_), One b ->
      let c = Trail.Age.compare a b in
      if c <> 0 then c else 1
    | Two (a1,b1), Two(a2,b2) ->
      let c = Trail.Age.compare a1 a2 in
      if c <> 0 then c else
      let c = Trail.Age.compare b1 b2 in
      c

  let invariant = function
    | No -> true
    | One _ -> true
    | Two (a,b) -> Trail.Age.compare a b >= 0

  let empty = No

  let add _ age = function
    | No -> One age
    | One age0 when Trail.Age.compare age0 age <= 0 -> Two(age,age0)
    | One age0 -> Two(age0,age)
    | Two(age0,_) when Trail.Age.compare age0 age <= 0 -> Two(age,age0)
    | Two(_,age1) as t when Trail.Age.compare age age1 <= 0 -> t
    | Two(age0,_)  -> Two(age0,age)

  let merge t1 = function
    | No -> t1
    | One age -> add () age t1
    | Two (age0,age1) -> add () age0 (add () age1 t1)

  let is_last_dec t = function
    | No -> false
    | One a | Two (a,_) -> Trail.Age.equal (Trail.last_dec t) a

  let before_last_dec t = function
    | No -> true
    | One a | Two (a,_) -> Trail.before_last_dec t a

  let before_first_dec t = function
    | No -> true
    | One a | Two (a,_) -> Trail.before_first_dec t a

  let at_most_one_after_last_dec t = function
    | No -> true
    | One _ -> true
    | Two (_,b) -> Trail.before_last_dec t b

  let get_second_last = function
    | No | One _ -> Trail.Age.min
    | Two(_,b) -> b

  let get_last = function
    | No -> assert false
    | One a
    | Two(a,_) -> a

end

module Cho = Trail.Cho

type 'd decdone  =
| DecNo
| DecTodo of 'd

module type Cho = sig
  module OnWhat  : Stdlib.Datatype

  val choose_decision:
    Egraph.Delayed.t -> OnWhat.t -> (Egraph.Delayed.t -> unit) decdone

  val key: OnWhat.t Cho.t

end

module ChoRegistry = Cho.Make_Registry(struct
    type 'a data = (module Cho with type OnWhat.t = 'a)
    let pp (type a) (cho: a data) =
      let module Cho = (val cho) in
      Cho.OnWhat.pp
    let key (type a) (cho: a data) =
      let module Cho = (val cho) in
      Cho.key
  end)

let register_cho (type a) (cho:(module Cho with type OnWhat.t = a)) =
  let module Cho = (val cho) in
  let cho = (module Cho : Cho with type OnWhat.t = Cho.OnWhat.t) in
  ChoRegistry.register cho

let choose_decision d (Trail.GCho(_,cho,c)) =
  let module C = (val ChoRegistry.get cho) in
  C.choose_decision d c

module ChoGenH = Stdlib.XHashtbl.Make(struct
    type t = Trail.chogen
    let hash = function
      | Trail.GCho (n,cho,k) ->
        let module C = (val ChoRegistry.get cho) in
        Hashcons.combine2 (Typedef.Node.hash n) (Cho.hash cho) (C.OnWhat.hash k)

    let equal (Trail.GCho(n1,cho1,k1)) (Trail.GCho(n2,cho2,k2)) =
      Typedef.Node.equal n1 n2 &&
      match Cho.Eq.eq_type cho1 cho2 with
      | Some Keys.Eq ->
        let f (type a) (cho: a Cho.t) k1 k2 =
          let module C = (val ChoRegistry.get cho) in
          C.OnWhat.equal k1 k2
        in
        f cho1 k1 k2
      | None -> false
  end)

module Conflict = struct
  type t = Trail.t

  let age_merge t n1 n2 = Trail.age_merge t n1 n2

end

module Exp = Trail.Exp
module Con = Trail.Con

module type Exp = sig

  type t

  val pp: t Pp.pp

  val key: t Trail.Exp.t


  val from_contradiction:
    Conflict.t (* -> Trail.Age.t *) -> t -> Trail.Pcon.t list
    (** First step of the analysis done on the trail. *)

  val analyse  :
    Conflict.t (* -> Trail.Age.t *) -> t -> 'a Con.t -> 'a -> Trail.Pcon.t list

end

module ExpRegistry = Exp.Make_Registry(struct
    type 'a data = (module Exp with type t = 'a)
    let pp (type a) (exp: a data) =
      let module Exp = (val exp) in
      Exp.pp
    let key (type a) (exp: a data) =
      let module Exp = (val exp) in
      Exp.key
  end)

let register_exp (exp:(module Exp)) =
  let module Exp = (val exp) in
  let exp = (module Exp : Exp with type t = Exp.t) in
  ExpRegistry.register exp


type parity = | Neg | Pos

module type Con = sig

  type t

  val pp: t Pp.pp

  val key: t Trail.Con.t

  val apply_learnt: t -> Typedef.Node.t * parity

  val levels: Conflict.t -> t -> Levels.t

  val useful_nodes: t -> Typedef.Node.t Bag.t

end

module ConRegistry = Con.Make_Registry(struct
    type 'a data = (module Con with type t = 'a)
    let pp (type a) (con: a data) =
      let module Con = (val con) in
      Con.pp
    let key (type a) (con: a data) =
      let module Con = (val con) in
      Con.key
  end)

let register_con (con:(module Con)) =
  let module Con = (val con) in
  let con = (module Con : Con with type t = Con.t) in
  ConRegistry.register con

let convert_lcon t l =
  let map (type a) con (c:a) pc =
    let module Con = (val (ConRegistry.get con) : Con with type t = a) in
    (Con.levels t c, pc)
  in
  List.map (fun (Trail.Pcon.Pcon(con,c) as pc) -> map con c pc) l

let compare_level_con (l1,_) (l2,_) = - (Levels.compare l1 l2)

let sort_lcon l = List.sort compare_level_con l

let merge_lcon l1 l2 = List.merge compare_level_con l1 l2

let lcon_to_node l =
  let map (type a) con (c:a) =
    let module Con = (val (ConRegistry.get con) : Con with type t = a) in
    Con.apply_learnt c
  in
  List.map (fun (Trail.Pcon.Pcon(con,c)) -> map con c) l

let _or = ref (fun _ -> assert false)
let _set_true = ref (fun _ _ _ -> assert false)
let apply_learnt d n =
  Egraph.Delayed.register d n;
  !_set_true d Trail.pexp_fact n

module Learnt = Typedef.Node

let print_pcon fmt (Trail.Pcon.Pcon(con,c)) =
  (ConRegistry.print con) fmt c

let print_lcon fmt (l,pcon) =
  Format.fprintf fmt "%a[%a]" print_pcon pcon Levels.pp l

let learn trail (Trail.Pexp.Pexp(_,exp,x)) =
  Debug.dprintf0 debug "[Conflict] @[Learning@]";
  let t = trail in
  let module Exp = (val (ExpRegistry.get exp)) in
  let l = Exp.from_contradiction trail x in
  let lcon = sort_lcon (convert_lcon t l) in
  Debug.dprintf2 debug "[Conflict] @[Initial conflict:%a@]"
    (Pp.list Pp.comma print_lcon) lcon;
  let analysis_done lv l =
    let backtrack_level = Levels.get_second_last lv in
    Debug.dprintf4 debug "[Conflict] @[End analysis with (bl %a): %a@]"
      Trail.Age.pp backtrack_level
      (Pp.list Pp.comma print_lcon) l;
    (** remove the one before the first decision (level 0)) *)
    let l = List.fold_left (fun acc (lv,c) ->
        if Levels.before_first_dec t lv then acc else c::acc
      ) [] l in
    (backtrack_level, l)
  in
  let rec aux = function
    | [] -> assert false (** absurd: understand why *)
    | (Levels.No,_)::_                           -> assert false (** absurd: understand when *)
    | (l1,_)::_ when Levels.before_last_dec t l1 -> assert false (** absurd: understand when *)
    | (l1,_)::_ as l when Levels.is_last_dec t l1 ->
      let lv = List.fold_left (fun acc (lv,_) -> Levels.merge lv acc) Levels.No l in
      analysis_done lv l
    | (l1,_)::[] as l when Levels.at_most_one_after_last_dec t l1 ->
      analysis_done l1 l
    | (l1,_)::(l2,_)::_ as l when Levels.at_most_one_after_last_dec t l1 && Levels.before_last_dec t l2 ->
      analysis_done (Levels.merge l1 l2) l
    | (l1,Trail.Pcon.Pcon(con,c))::lcon ->
      let age = Levels.get_last l1 in
      let f (type a) exp (e:a) =
        let module Exp = (val (ExpRegistry.get exp) : Exp with type t = a) in
        Debug.dprintf6 debug "[Conflict] @[[%a] Analyse using %a: %a@]"
          Trail.Age.pp age Exp.pp e (ConRegistry.print con) c;
        let res = Exp.analyse t e con c in
        res
      in
      let (Trail.Pexp.Pexp(_,exp,e)) = Trail.get_pexp t age in
      let lcon' = f exp e in
      let lcon' = convert_lcon t lcon' in
        Debug.dprintf2 debug "[Conflict] @[Analyse resulted in: %a@]"
          (Pp.list Pp.comma print_lcon) lcon';
      let lcon = merge_lcon (sort_lcon lcon') lcon in
      aux lcon
  in
  let backtrack_level, l = aux lcon in
  let fold useful (type a) (con:a Con.t) (c:a) =
    let module Con = (val ConRegistry.get con) in
    Bag.concat useful (Con.useful_nodes c)
  in
  let useful =
    List.fold_left
      (fun acc (Trail.Pcon.Pcon(con,c)) -> fold acc con c)
      Bag.empty l
  in
  let l = lcon_to_node l in
  backtrack_level, !_or l, useful


module EqCon = struct

  type t = {
    l: Typedef.Node.t;
    r: Typedef.Node.t;
  }

  let pp fmt c = Format.fprintf fmt
      "%a =@, %a"
      Typedef.Node.pp c.l Typedef.Node.pp c.r

  let key : t Con.t = Con.create_key "eq"

  let reg_apply_learnt = Ty.H.create 16

  let register_apply_learnt ty (f:(t -> Typedef.Node.t * parity)) =
    Ty.H.add reg_apply_learnt ty f

  let levels t c =
    let age = Conflict.age_merge t c.l c.r in
    Levels.add t age Levels.empty

  let useful_nodes c = Bag.list [c.l;c.r]

  let not_found = Invalid_argument "Type not found in apply_learnt EqCon"

  let apply_learnt c =
    let f = Ty.H.find_exn reg_apply_learnt not_found (Typedef.Node.ty c.l) in
    f c

  let split t c a b =
    let open Typedef in
    if Node.equal c.l a
    then [{ l = b; r = c.r }]
    else if Node.equal c.l b
    then [{ l = a; r = c.r }]
    else if Node.equal c.r a
    then [{ l = c.l; r = b }]
    else if Node.equal c.r b
    then [{ l = c.l; r = a }]
    else
      let age_a = Conflict.age_merge t c.l a in
      let age_b = Conflict.age_merge t c.l b in
      let cmp = Trail.Age.compare age_a age_b in
      assert (cmp <> 0);
      if cmp < 0
      then [{ l = c.l; r = a }; { l = b; r = c.r }]
      else [{ l = c.l; r = b }; { l = a; r = c.r }]
end

let () = register_con(module EqCon)

module Specific = struct


  let () = register_exp (module struct
      type t = unit
      let key = Trail.exp_fact

      let pp fmt () = Pp.string fmt "fact"

      let analyse _ _ _ _ = []
      let from_contradiction _ _ =
        raise Std.Impossible
    end)

  let () = register_exp (module struct
      type t = Typedef.Values.t * Typedef.Node.t * Typedef.Node.t * Typedef.Values.t * Trail.Pexp.t
      let key = Trail.exp_diff_value

      let pp fmt (v1,n1,n2,v2,Trail.Pexp.Pexp(_,exp,e)) =
        Format.fprintf fmt "diff_value(%a=%a=%a=%a):%a"
          Typedef.Values.pp v1 Typedef.Node.pp n1
          Typedef.Node.pp n2 Typedef.Values.pp v2
          (ExpRegistry.print exp) e

      let analyse _ _ _ _ = raise Std.Impossible (** used only for contradiction *)
      let from_contradiction t (v1,n1,n2,v2,Trail.Pexp.Pexp(_,exp,e)) =
        let f (type a) (exp:a Exp.t) e =
          let module Exp = (val (ExpRegistry.get exp)) in
          Debug.dprintf6 debug "[Conflict] @[Intermediary conflict diff value %a and %a: %a@]"
            Typedef.Node.pp n1 Typedef.Node.pp n2 Exp.pp e;
          Exp.analyse t e EqCon.key {l=n1;r=n2}
        in
        (** splitting of the equality v1 = v2 *)
        (Trail.Pcon.pcon EqCon.key {l=Typedef.Values.node v1;r=n1})::
        (Trail.Pcon.pcon EqCon.key {l=n2;r=Typedef.Values.node v2})::
        f exp e
    end)

  let () = register_exp (module struct
      type t = Trail.exp_same_sem
      let key = Trail.exp_same_sem

      let pp fmt = function
        | Trail.ExpSameSem(Trail.Pexp.Pexp(_,exp,e),n,th) ->
          Format.fprintf fmt "same_sem(%a,%a):%a"
            Typedef.Node.pp n Typedef.ThTerm.pp th (ExpRegistry.print exp) e
        | Trail.ExpSameValue(Trail.Pexp.Pexp(_,exp,e),n,value) ->
          Format.fprintf fmt "same_value(%a,%a):%a"
            Typedef.Node.pp n Typedef.Values.pp value (ExpRegistry.print exp) e

      let analyse t p con c =
        let Trail.Pexp.Pexp(_,exp,e) =
          match p with
          | Trail.ExpSameSem(pexp,_,_)
          | Trail.ExpSameValue(pexp,_,_) -> pexp in
        let f (type a) (exp:a Exp.t) e =
          let module Exp = (val (ExpRegistry.get exp)) in
          Exp.analyse t e con c
        in
        f exp e


      let from_contradiction _ _ =
        raise Std.Impossible
    end)

end
