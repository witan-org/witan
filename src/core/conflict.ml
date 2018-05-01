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

open Witan_popop_lib
open Witan_core_structures
open Std
open Nodes

module Age = Trail.Age

let print_decision = Debug.register_info_flag
  ~desc:"for@ the@ printing@ of@ the@ decisions@ done."
  "decisions"

let debug = Debug.register_info_flag
  ~desc:"for@ the@ main@ part@ of@ conflicts."
  "Conflict.core"

module Levels = struct
  type t =
    | No
    | One of Age.t
    | Two of Age.t * Age.t
  [@@ deriving eq, show]

  let pp fmt = function
    | No -> Format.string fmt "-"
    | One age -> Age.pp fmt age
    | Two (age1,age2) -> Format.fprintf fmt "%a,%a" Age.pp age1 Age.pp age2

  let compare l1 l2 =
    match l1, l2 with
    | No, No -> 0
    | _, No -> 1
    | No, _ -> -1
    | One a, One b -> Age.compare a b
    | One a, Two (b,_) ->
      let c = Age.compare a b in
      if c <> 0 then c else -1
    | Two (a,_), One b ->
      let c = Age.compare a b in
      if c <> 0 then c else 1
    | Two (a1,b1), Two(a2,b2) ->
      let c = Age.compare a1 a2 in
      if c <> 0 then c else
      let c = Age.compare b1 b2 in
      c

  let invariant = function
    | No -> true
    | One _ -> true
    | Two (a,b) -> Age.compare a b >= 0

  let empty = No

  let add _ age = function
    | No -> One age
    | One age0 when Age.compare age0 age <= 0 -> Two(age,age0)
    | One age0 -> Two(age0,age)
    | Two(age0,_) when Age.compare age0 age <= 0 -> Two(age,age0)
    | Two(_,age1) as t when Age.compare age age1 <= 0 -> t
    | Two(age0,_)  -> Two(age0,age)

  let merge t1 = function
    | No -> t1
    | One age -> add () age t1
    | Two (age0,age1) -> add () age0 (add () age1 t1)

  let is_last_dec t = function
    | No -> false
    | One a | Two (a,_) -> Age.equal (Trail.last_dec t) a

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
    | No | One _ -> Age.min
    | Two(_,b) -> b

  let get_last = function
    | No -> assert false
    | One a
    | Two(a,_) -> a

  let get_before_last_dec t = function
    | No -> Trail.Age.min
    | One a -> a
    | Two (a,_) when Trail.before_last_dec t a -> a
    | Two (_,b) ->
      assert (Trail.before_last_dec t b);
      b

end

module Cho = Trail.Cho

type 'd decdone  =
| DecNo
| DecTodo of 'd

module type Cho = sig
  module OnWhat  : Stdlib.Datatype

  val choose_decision:
    Egraph.t -> OnWhat.t -> (Egraph.t -> unit) decdone

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
        Hashcons.combine2 (Node.hash n) (Cho.hash cho) (C.OnWhat.hash k)

    let equal (Trail.GCho(n1,cho1,k1)) (Trail.GCho(n2,cho2,k2)) =
      Node.equal n1 n2 &&
      match Cho.Eq.eq_type cho1 cho2 with
      | Poly.Eq ->
        let f (type a) (cho: a Cho.t) k1 k2 =
          let module C = (val ChoRegistry.get cho) in
          C.OnWhat.equal k1 k2
        in
        f cho1 k1 k2
      | Poly.Neq -> false
  end)

module Exp = Trail.Exp
module Hyp = Trail.Hyp

module TrailCache = Stdlib.MkDatatype(struct
    type t = Node.t * Node.t
    [@@ deriving eq, show, ord]

    let hash (a,b) = CCHash.combine2 (Node.hash a) (Node.hash b)
  end)

type conflict = {
  getter: Egraph.Getter.t;
  trail : Trail.t;
  trail_cache : Trail.Age.t option TrailCache.H.t;
  mutable index: int;
  mutable nb_in_todolist : int;
  todolist : (Trail.Age.t * Trail.Phyp.t) list array;
  mutable fromdec  : Trail.Phyp.t list;
  mutable levels_before_last_dec : Levels.t;
  mutable before_last_dec : Trail.Phyp.t list;
  mutable before_first_dec : Trail.Phyp.t list;
}

let create_env getter trail =
  let size = 1 + Age.to_int (Trail.current_age trail) - Age.to_int (Trail.last_dec trail)  in
  {
    getter;
    trail;
    trail_cache = TrailCache.H.create 256;
    index = size-1;
    nb_in_todolist = 0;
    todolist = Array.make size [];
    fromdec = [];
    levels_before_last_dec = Levels.No;
    before_last_dec = [];
    before_first_dec = [];
  }

module type Exp = sig

  type t

  val pp: t Format.printer

  val key: t Trail.Exp.t


  val from_contradiction:
    conflict (* -> Age.t *) -> t -> Trail.Phyp.t list
    (** First step of the analysis done on the trail. *)

  val analyse  :
    conflict (* -> Age.t *) -> t -> Trail.Phyp.t -> Trail.Phyp.t list

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

let pp_pexp fmt (Trail.Pexp.Pexp(_,exp,e)) = ExpRegistry.print exp fmt e

let () = Trail._pp_pexp := pp_pexp

type parity = | Neg | Pos
let neg_parity = function | Neg -> Pos | Pos -> Neg

module type Hyp = sig

  type t

  val pp: t Format.printer

  val key: t Trail.Hyp.t

  val apply_learnt: t -> Node.t * parity

  val levels: conflict -> t -> Levels.t

  val useful_nodes: t -> Node.t Bag.t

  val split: conflict -> t -> Node.t -> Node.t -> Trail.Phyp.t list

end

module HypRegistry = Hyp.Make_Registry(struct
    type 'a data = (module Hyp with type t = 'a)
    let pp (type a) (hyp: a data) =
      let module Hyp = (val hyp) in
      Hyp.pp
    let key (type a) (hyp: a data) =
      let module Hyp = (val hyp) in
      Hyp.key
  end)

let register_hyp (hyp:(module Hyp)) =
  let module Hyp = (val hyp) in
  let hyp = (module Hyp : Hyp with type t = Hyp.t) in
  HypRegistry.register hyp

let pp_phyp fmt (Trail.Phyp.Phyp(hyp,c,_)) =
  (HypRegistry.print hyp) fmt c

let pp_lhyp fmt (l,phyp) =
  Format.fprintf fmt "%a[%a]" pp_phyp phyp Levels.pp l

module Conflict = struct

  type t = conflict

  let getter x = x.getter

  let age_merge_opt t n1 n2 =
    TrailCache.H.memo
      (fun (n1,n2) -> Trail.age_merge_opt t.trail n1 n2)
      t.trail_cache (n1,n2)

  exception NeverMerged of Node.t * Node.t

  let age_merge t n1 n2 =
    match age_merge_opt t n1 n2 with
    | Some a -> a
    | None -> raise (NeverMerged(n1,n2))

  let analyse' (type a) t exp e pcon =
    let module Exp = (val (ExpRegistry.get exp) : Exp with type t = a) in
    Debug.dprintf4 debug "[Conflict] @[Analyse using %a: %a@]"
      (* Age.pp age *) Exp.pp e pp_phyp pcon;
    let res = Exp.analyse t e pcon in
    res

  let analyse t (Trail.Pexp.Pexp(_,exp,e)) pcon = analyse' t exp e pcon

  let split t (Trail.Phyp.Phyp(hyp,c,dec)) a b =
    assert (match dec with `NoDec -> true | _ -> false);
    let f (type a) (hyp: a Hyp.t) (c:a) =
      let module Hyp = (val (HypRegistry.get hyp)) in
      Hyp.split t c a b
    in
    f hyp c
end

let _or = ref (fun _ -> assert false)
let _set_true = ref (fun _ _ _ -> assert false)
let _is_true = ref (fun _ _ -> assert false)
let _equality = ref (fun _ _ -> assert false)

let apply_learnt d n =
  Egraph.register d n;
  !_set_true d Trail.pexp_fact n

let learnt_is_already_true d n =
  Egraph.is_registered d n &&
  !_is_true d n

module Learnt = Node


module TodoList : sig
  val merge: Conflict.t -> Trail.Phyp.t list -> unit

  type state =
    | Finish of (Age.t * (Node.t * parity) list * Node.t Bag.t)
    | Next of Age.t * Trail.Phyp.t

  val state: Conflict.t -> state
end = struct

  let convert t l =
    let map (type a) hyp (c:a) dec pc =
      match dec with
      | `Dec -> (Levels.No,pc)
      | _ ->
        let module Hyp = (val (HypRegistry.get hyp) : Hyp with type t = a) in
        (Hyp.levels t c, pc)
    in
    List.map (fun (Trail.Phyp.Phyp(hyp,c,dec) as pc) -> map hyp c dec pc) l

  let compare_level_hyp (l1,_) (l2,_) = - (Levels.compare l1 l2)

  let sort l = List.sort compare_level_hyp l

  let merge t l2 =
    Debug.dprintf2 debug "[Hypflict] @[Analyse resulted in: %a@]"
      Format.(list ~sep:(const char ',') pp_phyp) l2;
    let iter (type a) hyp (c:a) dec pc =
      match dec with
      | `Dec -> t.fromdec <- pc::t.fromdec
      | _ ->
        let module Hyp = (val (HypRegistry.get hyp) : Hyp with type t = a) in
        let lv = Hyp.levels t c in
        if Levels.before_first_dec t.trail lv
        then t.before_first_dec <- pc::t.before_first_dec
        else if Levels.before_last_dec t.trail lv
        then begin
          t.before_last_dec <- pc::t.before_last_dec;
          t.levels_before_last_dec <- Levels.merge lv t.levels_before_last_dec;
        end
        else
          let index = Age.to_int (Levels.get_last lv) - Age.to_int (Trail.last_dec t.trail) in
          t.todolist.(index) <- (Levels.get_second_last lv,pc)::t.todolist.(index);
    in
    List.iter (fun (Trail.Phyp.Phyp(hyp,c,dec) as pc) -> iter hyp c dec pc) l2

  let analysis_done t last =
    let lv,last = match last with
      | None -> t.levels_before_last_dec, []
      | Some (lv,c) -> Levels.add t lv t.levels_before_last_dec, [c] in
    let backtrack_level = Levels.get_before_last_dec t.trail lv in
    let l = List.rev_append last (List.rev_append t.fromdec t.before_last_dec) in
    Debug.dprintf4 debug "[Conflict] @[End analysis with (bl %a): %a@]"
      Age.pp backtrack_level
      Format.(list ~sep:(const char ',') pp_phyp) l;
    (backtrack_level, l)

  let to_nodes l =
    let map (type a) hyp (c:a) =
      let module Hyp = (val (HypRegistry.get hyp) : Hyp with type t = a) in
      Hyp.apply_learnt c
    in
    List.map (fun (Trail.Phyp.Phyp(hyp,c,_)) -> map hyp c) l

  type state =
    | Finish of (Age.t * (Node.t * parity) list * Node.t Bag.t)
    | Next of Age.t * Trail.Phyp.t

  let finish t last =
    let backtrack_level, l = analysis_done t last in
    let fold useful (type a) (hyp:a Hyp.t) (c:a) =
      let module Hyp = (val HypRegistry.get hyp) in
      Bag.concat useful (Hyp.useful_nodes c)
    in
    let useful =
      List.fold_left
        (fun acc (Trail.Phyp.Phyp(hyp,c,_)) -> fold acc hyp c)
        Bag.empty l
    in
    let l = to_nodes l in
    (** The clause is the negation of the conjunction of the hypothesis *)
    let l = List.map (fun (c,p) -> (c,match p with | Neg -> Pos | Pos -> Neg)) l in
    Finish (backtrack_level, l, useful)

  let rec state t =
    if t.index = -1
    then finish t None (** a decision that directly make a conflict *)
    else
      match t.todolist.(t.index) with
      | [] ->
        if t.index = 0
        then finish t None
        else begin t.index <- t.index - 1; state t end
      | ((a,_) as p)::_ when t.nb_in_todolist = 1 && Trail.before_last_dec t.trail a ->
        finish t (Some p)
      | (_,c)::l ->
        t.nb_in_todolist <- t.nb_in_todolist - 1;
        t.todolist.(t.index) <- l;
        Next(Age.of_int (t.index + (Age.to_int (Trail.last_dec t.trail))), c)
end

let learn getter trail (Trail.Pexp.Pexp(_,exp,x) as pexp) =
  Debug.dprintf0 debug "[Conflict] @[Learning@]";
  let t = create_env getter trail in
  let module Exp = (val (ExpRegistry.get exp)) in
  Debug.dprintf2 debug "[Conflict] @[The contradiction: %a@]"
    pp_pexp pexp;
  let l = Exp.from_contradiction t x in
  TodoList.merge t l;
  let rec aux t =
    match TodoList.state t with
    | Finish(backtrack,l,useful) -> backtrack,!_or l,useful
    | Next(age,phyp) ->
      let pexp = Trail.get_pexp t.trail age in
      let lhyp' = Conflict.analyse t pexp phyp in
      TodoList.merge t lhyp';
      aux t
  in
  aux t


module EqHyp = struct

  type t = {
    l: Node.t;
    r: Node.t;
  }

  let pp fmt c = Format.fprintf fmt
      "%a =@, %a"
      Node.pp c.l Node.pp c.r

  let key : t Hyp.t = Hyp.create_key (module struct type nonrec t = t let name = "eq" end)

  let reg_apply_learnt = Ty.H.create 16

  let register_apply_learnt ty (f:(t -> Node.t * parity)) =
    Ty.H.add reg_apply_learnt ty f

  let levels t c =
    let age = Conflict.age_merge t c.l c.r in
    Levels.add t age Levels.empty

  let useful_nodes c = Bag.list [c.l;c.r]

  let not_found = Invalid_argument "Type not found in apply_learnt EqHyp"

  let apply_learnt c =
    match Ty.H.find_opt reg_apply_learnt (Node.ty c.l) with
    | None ->
      !_equality c.l c.r, Pos
    | Some f -> f c

  let split t c a b =
    if Node.equal c.l a
    then None, Some b
    else if Node.equal c.l b
    then None, Some a
    else if Node.equal c.r a
    then Some b, None
    else if Node.equal c.r b
    then Some a, None
    else
      match Conflict.age_merge_opt t c.l a,
            Conflict.age_merge_opt t c.l b with
      | Some age_a, Some age_b ->
        let cmp = Age.compare age_a age_b in
        assert (cmp <> 0);
        if cmp < 0
        then Some a, Some b
        else Some b, Some a
      | Some _, None ->
        (** They must be equal to one of them *)
        assert (not(Equal.option Age.equal (Conflict.age_merge_opt t c.r b) None));
        Some a, Some b
      | None, Some _ ->
        (** They must be equal to one of them *)
        assert (not(Equal.option Age.equal (Conflict.age_merge_opt t c.r a) None));
        Some b, Some a
      | None, None ->
        assert false (** absurd: One of them must be equal with the left *)

  let orient_split t c a b =
    let age_a = Conflict.age_merge t c.l a in
    let age_b = Conflict.age_merge t c.l b in
    let cmp = Age.compare age_a age_b in
    assert (cmp <> 0);
    if cmp < 0
    then a, b
    else b, a

  let create_eq ?dec l r =
    if Node.equal l r
    then []
    else [Trail.Phyp.phyp ?dec key {l;r}]

end

let () = register_hyp(module struct
    include EqHyp

    let split t c a b =
      let l', r' = split t c a b in
      Trail.Phyp.map key begin
      (match l' with
       | None -> []
       | Some r -> [{l=c.l; r}])
      @
      (match r' with
       | None -> []
       | Some l -> [{l; r=c.r}])
    end
  end)

module Specific = struct


  let () = register_exp (module struct
      type t = unit
      let key = Trail.exp_fact

      let pp fmt () = Format.string fmt "fact"

      let analyse _ _ _ = []
      let from_contradiction _ _ =
        raise Std.Impossible
    end)

  let () = register_exp (module struct
      type t = Value.t * Node.t * Node.t * Value.t * Trail.Pexp.t
      let key = Trail.exp_diff_value

      let pp fmt (v1,n1,n2,v2,Trail.Pexp.Pexp(_,exp,e)) =
        Format.fprintf fmt "diff_value(%a=%a=%a=%a):%a"
          Value.pp v1 Node.pp n1
          Node.pp n2 Value.pp v2
          (ExpRegistry.print exp) e

      let analyse _ _ _ = raise Std.Impossible (** used only for contradiction *)
      let from_contradiction t (v1,n1,n2,v2,pexp) =
        Debug.dprintf10 debug "[Conflict] @[Intermediary conflict diff value %a=%a=%a=%a: %a@]"
          Value.pp v1 Node.pp n1 Node.pp n2 Value.pp v2 pp_pexp pexp;
        if false
        then
          (** splitting of the equality v1 = v2 *)
          let phyp1 = (Trail.Phyp.phyp EqHyp.key {l=Value.node v1;r=n1}) in
          let phyp2 = (Trail.Phyp.phyp EqHyp.key {l=n2;r=Value.node v2}) in
          (* if Trail.before_last_dec t (Trail.age_merge t (Values.node v1) n1) &&
           *    Trail.before_last_dec t (Trail.age_merge t (Values.node v2) n2)
           * then (\** An inverse propagation not powerful enough *\)
           *   pcon1::pcon2::(EqCon.create_eq n1 n2)
           * else *)
          phyp1::phyp2::(Conflict.analyse t pexp (Trail.Phyp.phyp EqHyp.key {l=n1;r=n2}))
        else
          (** without splitting *)
          (Conflict.analyse t pexp (Trail.Phyp.phyp EqHyp.key {l=Value.node v1;r=Value.node v2}))
    end)

  let () = register_exp (module struct
      type t = Trail.exp_same_sem
      let key = Trail.exp_same_sem

      let pp fmt = function
        | Trail.ExpSameSem(Trail.Pexp.Pexp(_,exp,e),n,th) ->
          Format.fprintf fmt "same_sem(%a,%a):%a"
            Node.pp n ThTerm.pp th (ExpRegistry.print exp) e
        | Trail.ExpSameValue(Trail.Pexp.Pexp(_,exp,e),n,value) ->
          Format.fprintf fmt "same_value(%a,%a):%a"
            Node.pp n Value.pp value (ExpRegistry.print exp) e

      let analyse t p phyp =
        let Trail.Pexp.Pexp(_,exp,e) =
          match p with
          | Trail.ExpSameSem(pexp,_,_)
          | Trail.ExpSameValue(pexp,_,_) -> pexp in
        let f (type a) (exp:a Exp.t) e =
          let module Exp = (val (ExpRegistry.get exp)) in
          Exp.analyse t e phyp
        in
        f exp e


      let from_contradiction _ _ =
        raise Std.Impossible
    end)

end


let () = Exn_printer.register (fun fmt exn ->
    match exn with
    | Conflict.NeverMerged(n1,n2) ->
      Format.fprintf fmt "age_merge: node %a and %a have not been merged."
        Node.pp n1 Node.pp n2
    | exn -> raise exn
  )


let check_initialization () =
  HypRegistry.is_well_initialized ()
  && ExpRegistry.is_well_initialized ()
  && ChoRegistry.is_well_initialized ()
