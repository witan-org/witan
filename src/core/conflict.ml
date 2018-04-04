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
open Typedef

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
  [@@ deriving eq]

  let pp fmt = function
    | No -> Pp.string fmt "-"
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
        Hashcons.combine2 (Node.hash n) (Cho.hash cho) (C.OnWhat.hash k)

    let equal (Trail.GCho(n1,cho1,k1)) (Trail.GCho(n2,cho2,k2)) =
      Node.equal n1 n2 &&
      match Cho.Eq.eq_type cho1 cho2 with
      | Some Keys.Eq ->
        let f (type a) (cho: a Cho.t) k1 k2 =
          let module C = (val ChoRegistry.get cho) in
          C.OnWhat.equal k1 k2
        in
        f cho1 k1 k2
      | None -> false
  end)

module Exp = Trail.Exp
module Con = Trail.Con

module TrailCache = Stdlib.MkDatatype(struct
    type t = Node.t * Node.t
    [@@ deriving eq, show, ord]

    let hash (a,b) = CCHash.combine2 (Node.hash a) (Node.hash b)
  end)

type conflict = {
  trail : Trail.t;
  trail_cache : Trail.Age.t option TrailCache.H.t;
  mutable index: int;
  mutable nb_in_todolist : int;
  todolist : (Trail.Age.t * Trail.Pcon.t) list array;
  mutable fromdec  : Trail.Pcon.t list;
  mutable levels_before_last_dec : Levels.t;
  mutable before_last_dec : Trail.Pcon.t list;
  mutable before_first_dec : Trail.Pcon.t list;
}

let create_env trail =
  Debug.dprintf4 debug "create_env %a %a"
    Trail.Age.pp (Trail.current_age trail)
    Trail.Age.pp (Trail.last_dec trail);
  let size = 1 + Age.to_int (Trail.current_age trail) - Age.to_int (Trail.last_dec trail)  in
  {
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

  val pp: t Pp.pp

  val key: t Trail.Exp.t


  val from_contradiction:
    conflict (* -> Age.t *) -> t -> Trail.Pcon.t list
    (** First step of the analysis done on the trail. *)

  val analyse  :
    conflict (* -> Age.t *) -> t -> Trail.Pcon.t -> Trail.Pcon.t list

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

type parity = | Neg | Pos
let neg_parity = function | Neg -> Pos | Pos -> Neg

module type Con = sig

  type t

  val pp: t Pp.pp

  val key: t Trail.Con.t

  val apply_learnt: t -> Node.t * parity

  val levels: conflict -> t -> Levels.t

  val useful_nodes: t -> Node.t Bag.t

  val split: conflict -> t -> Node.t -> Node.t -> Trail.Pcon.t list

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

let pp_pcon fmt (Trail.Pcon.Pcon(con,c,_)) =
  (ConRegistry.print con) fmt c

let pp_lcon fmt (l,pcon) =
  Format.fprintf fmt "%a[%a]" pp_pcon pcon Levels.pp l

module Conflict = struct

  type t = conflict

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
      (* Age.pp age *) Exp.pp e pp_pcon pcon;
    let res = Exp.analyse t e pcon in
    res

  let analyse t (Trail.Pexp.Pexp(_,exp,e)) pcon = analyse' t exp e pcon

  let split t (Trail.Pcon.Pcon(con,c,dec)) a b =
    assert (dec = `NoDec);
    let f (type a) (con: a Con.t) (c:a) =
      let module Con = (val (ConRegistry.get con)) in
      Con.split t c a b
    in
    f con c
end

let _or = ref (fun _ -> assert false)
let _set_true = ref (fun _ _ _ -> assert false)
let _equality = ref (fun _ _ -> assert false)

let apply_learnt d n =
  Egraph.Delayed.register d n;
  !_set_true d Trail.pexp_fact n

module Learnt = Node


module TodoList : sig
  val merge: Conflict.t -> Trail.Pcon.t list -> unit

  type state =
    | Finish of (Age.t * (Node.t * parity) list * Node.t Bag.t)
    | Next of Age.t * Trail.Pcon.t

  val state: Conflict.t -> state
end = struct

  let convert t l =
    let map (type a) con (c:a) dec pc =
      if dec = `Dec then (Levels.No,pc)
      else
        let module Con = (val (ConRegistry.get con) : Con with type t = a) in
        (Con.levels t c, pc)
    in
    List.map (fun (Trail.Pcon.Pcon(con,c,dec) as pc) -> map con c dec pc) l

  let compare_level_con (l1,_) (l2,_) = - (Levels.compare l1 l2)

  let sort l = List.sort compare_level_con l

  let merge t l2 =
    Debug.dprintf2 debug "[Conflict] @[Analyse resulted in: %a@]"
      (Pp.list Pp.comma pp_pcon) l2;
    let iter (type a) con (c:a) dec pc =
      if dec = `Dec
      then t.fromdec <- pc::t.fromdec
      else
        let module Con = (val (ConRegistry.get con) : Con with type t = a) in
        let lv = Con.levels t c in
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
    List.iter (fun (Trail.Pcon.Pcon(con,c,dec) as pc) -> iter con c dec pc) l2

  let analysis_done t last =
    let lv,last = match last with
      | None -> t.levels_before_last_dec, []
      | Some (lv,c) -> Levels.add t lv t.levels_before_last_dec, [c] in
    let backtrack_level = Levels.get_second_last lv in
    let l = List.rev_append last (List.rev_append t.fromdec t.before_last_dec) in
    Debug.dprintf4 debug "[Conflict] @[End analysis with (bl %a): %a@]"
      Age.pp backtrack_level
      (Pp.list Pp.comma pp_pcon) l;
    (backtrack_level, l)

  let to_nodes l =
    let map (type a) con (c:a) =
      let module Con = (val (ConRegistry.get con) : Con with type t = a) in
      Con.apply_learnt c
    in
    List.map (fun (Trail.Pcon.Pcon(con,c,_)) -> map con c) l

  type state =
    | Finish of (Age.t * (Node.t * parity) list * Node.t Bag.t)
    | Next of Age.t * Trail.Pcon.t

  let finish t last =
    let backtrack_level, l = analysis_done t last in
    let fold useful (type a) (con:a Con.t) (c:a) =
      let module Con = (val ConRegistry.get con) in
      Bag.concat useful (Con.useful_nodes c)
    in
    let useful =
      List.fold_left
        (fun acc (Trail.Pcon.Pcon(con,c,_)) -> fold acc con c)
        Bag.empty l
    in
    let l = to_nodes l in
    (** The clause is the negation of the conjunction of the hypothesis *)
    let l = List.map (fun (c,p) -> (c,match p with | Neg -> Pos | Pos -> Neg)) l in
    Finish (backtrack_level, l, useful)

  let rec state t =
    Debug.dprintf2 debug "state: %i %i" t.index (Array.length t.todolist);
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

let learn trail (Trail.Pexp.Pexp(_,exp,x) as pexp) =
  Debug.dprintf0 debug "[Conflict] @[Learning@]";
  let t = create_env trail in
  let module Exp = (val (ExpRegistry.get exp)) in
  Debug.dprintf2 debug "[Conflict] @[The contradiction: %a@]"
    pp_pexp pexp;
  let l = Exp.from_contradiction t x in
  TodoList.merge t l;
  let rec aux t =
    match TodoList.state t with
    | Finish(backtrack,l,useful) -> backtrack,!_or l,useful
    | Next(age,pcon) ->
      let pexp = Trail.get_pexp t.trail age in
      let lcon' = Conflict.analyse t pexp pcon in
      TodoList.merge t lcon';
      aux t
  in
  aux t


module EqCon = struct

  type t = {
    l: Node.t;
    r: Node.t;
  }

  let pp fmt c = Format.fprintf fmt
      "%a =@, %a"
      Node.pp c.l Node.pp c.r

  let key : t Con.t = Con.create_key "eq"

  let reg_apply_learnt = Ty.H.create 16

  let register_apply_learnt ty (f:(t -> Node.t * parity)) =
    Ty.H.add reg_apply_learnt ty f

  let levels t c =
    let age = Conflict.age_merge t c.l c.r in
    Levels.add t age Levels.empty

  let useful_nodes c = Bag.list [c.l;c.r]

  let not_found = Invalid_argument "Type not found in apply_learnt EqCon"

  let apply_learnt c =
    match Ty.H.find_opt reg_apply_learnt (Node.ty c.l) with
    | None ->
      !_equality c.l c.r, Pos
    | Some f -> f c

  let split t c a b =
    let open Typedef in
    if Node.equal c.l a
    then None, Some b
    else if Node.equal c.l b
    then None, Some a
    else if Node.equal c.r a
    then Some b, None
    else if Node.equal c.r b
    then Some a, None
    else
      let age_a = Conflict.age_merge t c.l a in
      let age_b = Conflict.age_merge t c.l b in
      let cmp = Age.compare age_a age_b in
      assert (cmp <> 0);
      if cmp < 0
      then Some a, Some b
      else Some b, Some a

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
    else [Trail.Pcon.pcon ?dec key {l;r}]

end

let () = register_con(module struct
    include EqCon

    let split t c a b =
      let l', r' = split t c a b in
      Trail.Pcon.map key begin
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

      let pp fmt () = Pp.string fmt "fact"

      let analyse _ _ _ = []
      let from_contradiction _ _ =
        raise Std.Impossible
    end)

  let () = register_exp (module struct
      type t = Values.t * Node.t * Node.t * Values.t * Trail.Pexp.t
      let key = Trail.exp_diff_value

      let pp fmt (v1,n1,n2,v2,Trail.Pexp.Pexp(_,exp,e)) =
        Format.fprintf fmt "diff_value(%a=%a=%a=%a):%a"
          Values.pp v1 Node.pp n1
          Node.pp n2 Values.pp v2
          (ExpRegistry.print exp) e

      let analyse _ _ _ = raise Std.Impossible (** used only for contradiction *)
      let from_contradiction t (v1,n1,n2,v2,Trail.Pexp.Pexp(_,exp,e)) =
        let f (type a) (exp:a Exp.t) e =
          let module Exp = (val (ExpRegistry.get exp)) in
          Debug.dprintf10 debug "[Conflict] @[Intermediary conflict diff value %a=%a=%a=%a: %a@]"
            Values.pp v1 Node.pp n1 Node.pp n2 Values.pp v2 Exp.pp e;
          Exp.analyse t e (Trail.Pcon.pcon EqCon.key {l=n1;r=n2})
        in
        (** splitting of the equality v1 = v2 *)
        let pcon1 = (Trail.Pcon.pcon EqCon.key {l=Values.node v1;r=n1}) in
        let pcon2 = (Trail.Pcon.pcon EqCon.key {l=n2;r=Values.node v2}) in
        (* if Trail.before_last_dec t (Trail.age_merge t (Values.node v1) n1) &&
         *    Trail.before_last_dec t (Trail.age_merge t (Values.node v2) n2)
         * then (\** An inverse propagation not powerful enough *\)
         *   pcon1::pcon2::(EqCon.create_eq n1 n2)
         * else *)
          pcon1::pcon2::(f exp e)
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
            Node.pp n Values.pp value (ExpRegistry.print exp) e

      let analyse t p pcon =
        let Trail.Pexp.Pexp(_,exp,e) =
          match p with
          | Trail.ExpSameSem(pexp,_,_)
          | Trail.ExpSameValue(pexp,_,_) -> pexp in
        let f (type a) (exp:a Exp.t) e =
          let module Exp = (val (ExpRegistry.get exp)) in
          Exp.analyse t e pcon
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
