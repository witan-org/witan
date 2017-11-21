(*************************************************************************)
(*  This file is part of Witan.                                          *)
(*                                                                       *)
(*  Copyright (C) 2017                                                   *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies           *)
(*           alternatives)                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en        *)
(*           Automatique)                                                *)
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

open Std
open Stdlib
open Typedef

(** The age is a position in the trail. So it is also a way to
    indicate the state. The state corresponding to an age is the state
    of the solver when the current age integer have been incremented
    into this age *)
let debugage = Debug.register_info_flag
    ~desc:"for@ the@ age in the trail."
    "Explanation.age"

let debug = Debug.register_flag (** not info just because duplicate of solver *)
  ~desc:"for@ the@ trail."
  "Explanation.core"

module Exp = Typedef.Make_key(struct end)
module Con = Typedef.Make_key(struct end)
module Cho = Typedef.Make_key2(struct end)

type 'a exp = 'a Exp.k
type 'a con = 'a Con.k
type ('a,'b) cho = ('a,'b) Cho.k

module Age = struct
  include DIntOrd
  let bef = -1
  let min = 0
  let max (x : int) y = if x < y then y else x
  let pred x = x - 1
  let succ x = x + 1
  let to_int x = x
end
type age = Age.t (* position in the trail *)

module Tag = Typedef.Make_key(struct end)
type 'a tag = 'a Tag.k

module Tags : sig
  type t
  val empty: t
  val add: t -> 'a tag -> 'a Bag.t -> t
  val find: t -> 'a tag -> 'a Bag.t
  val union: t -> t -> t
  val pp: t Pp.pp
end = struct
  type exi
  type t = exi Bag.t Tag.K.M.t
  let empty = Tag.K.M.empty
  let add : type a. t -> a tag -> a Bag.t -> t =
    fun tags tag l ->
      Tag.K.M.add ((tag : a tag) :> Tag.K.t)
        (Obj.magic (l : a Bag.t) :> exi Bag.t) tags
  let find : type a. t -> a tag -> a Bag.t =
    fun tags tag ->
      (Obj.magic (Tag.K.M.find_def Bag.empty ((tag : a tag) :> Tag.K.t)
                    tags : exi Bag.t) : a Bag.t)
  let union : t -> t -> t = fun t1 t2 ->
         Tag.K.M.union (fun _ a b -> Some (Bag.concat a b)) t1 t2
  let pp fmt _ = Format.pp_print_string fmt "!Tags!"
end
type tags = Tags.t

type dec = age
let age_of_dec x = x
let print_dec = Age.pp

type 'a rescon =
| GRequested: 'a -> 'a rescon
| GOther: 'b con * 'b ->  'a rescon

module Conunknown = Con.MkMap(struct type ('a,'b) t = 'a Bag.t end)
type conunknown = unit Conunknown.t
type chogen =
  | GCho: ('k,'d) cho * 'k -> chogen
type decs = chogen list Cl.M.t Dom.M.t

(** Module for manipulating explicit dependencies *)
module Deps = struct

  type t =
    | Empty
    | Tags: tags * (t ref) -> t
    | Decs: chogen * (t ref) -> t
    | Cunk: 'a con * 'a * (t ref) -> t
    | Concat: (t ref) * (t ref) -> t

  let empty = Empty
  let is_empty = function Empty -> true | _ -> false
  let concat t1 t2 =
    (* Format.fprintf (Debug.get_debug_formatter ()) "."; *)
    match t1,t2 with
    | Empty, Empty -> Empty
    | Empty, t | t, Empty -> t
    | _ -> Concat(ref t2,ref t1)
  let concatl l = List.fold_left concat Empty l
  let add_tags t tags =
    (* Format.fprintf (Debug.get_debug_formatter ()) "$"; *)
    Tags(tags,ref t)
  let add_unknown_con t con a =
    (* Format.fprintf (Debug.get_debug_formatter ()) "!%a" Con.pp con; *)
    Cunk(con,a,ref t)
  let add_chogen t chogen =
    (* Format.fprintf (Debug.get_debug_formatter ()) "*"; *)
    Decs(chogen,ref t)
end


module Deps_Result = struct
  open Deps
  type t = {
    unknown : conunknown;
    tags    : tags;
    decs    : chogen Bag.t;
  }

  let empty = {
    unknown = Conunknown.empty;
    tags    = Tags.empty;
    decs    = Bag.empty;
  }

  let concat t1 t2 =
    let conunion _ l1 l2 =
      Some (Bag.concat l1 l2) in
    {
      tags = Tags.union t1.tags t2.tags;
      unknown = Conunknown.union {Conunknown.union = conunion}
          t1.unknown t2.unknown;
      decs = Bag.concat t1.decs t2.decs;
    }

  let add_tags t tags =
    {t with tags = Tags.union t.tags tags}

  let add_unknown_con t con a =
    { t with unknown =
               Conunknown.add_change
                 Bag.elt Bag.add
                 con a t.unknown
    }

  let add_chogen t chogen =
    { t with decs = Bag.add chogen t.decs }

  let rec fold acc t =
    match t with
    | Empty -> acc
    | Tags(tags,t') ->
      let acc = fold acc !t' in
      t' := Empty;
      add_tags acc tags
    | Decs(chogen,t') ->
      let acc = fold acc !t' in
      t' := Empty;
      add_chogen acc chogen
    | Cunk(con,a,t') ->
      let acc = fold acc !t' in
      t' := Empty;
      add_unknown_con acc con a
    | Concat(t1,t2) ->
      let acc = fold acc !t1 in
      t1 := Empty;
      let acc = fold acc !t2 in
      t2 := Empty;
      acc

  let compute_deps t = fold empty t
end


module Concache = struct

  type 'a value = 'a rescon * Deps.t
  module Cache = Con.MkVector(struct
    type ('a,'b) t = 'a value end)

  type t = unit Cache.t

  let mk () = Cache.create (Con.hint_size ())
  let set (type a) (concache:t) (con:a con) (c:a value) =
    Cache.inc_size con concache;
    Cache.set concache con c
  let get concache con = Cache.get concache con
  let is_set concache con = not (Cache.is_uninitialized concache con)
  let clear = Cache.clear
end

type concache = Concache.t

type pexp =
| Pexp: age * 'a exp * 'a * tags * concache -> pexp

let pp_pexp_ref : (pexp Pp.pp) ref =
  ref (fun _ _ -> assert false)

let pp_pexp fmt x = !pp_pexp_ref fmt x
let show_pexp = Pp.string_of_wnl pp_pexp


type modif =
| Cl : Cl.t * Cl.t             -> modif (** Just for taking an age *)
| Dom: Cl.t * 'a dom      * pexp * Cl.t -> modif
| DomL: Cl.t * 'a dom * 'a option * Age.t * pexp * Cl.t -> modif
| Dec: dec                       -> modif

let print_modif_ref = ref (fun _ _ -> assert false)

type node_clhist = {
  nage : age;
  ncl : Cl.t;
  npexp: pexp;
  ninv : bool;
}

let print_node fmt e =
    Format.fprintf fmt "M(%a,@,%a,@,%a,@,%b)"
      Age.pp e.nage Cl.pp e.ncl pp_pexp e.npexp e.ninv


type clgraph = (node_clhist list) Cl.M.t (** graph *)
type clhist = (age * Cl.t) Cl.M.t (** graph *)

type mod_dom = {
  modcl : Cl.t;
  modage : Age.t;
  modpexp : pexp
}

let print_mod_dom fmt m =
  Format.fprintf fmt "{cl=%a;@,age=%a;@,pexp=%a}"
    Cl.pp m.modcl Age.pp m.modage pp_pexp m.modpexp

type domhist_node =
  | DomNeverSet
  | DomMerge of Age.t (** agedommerge *) *
                domhist_node (** other_cl *) * domhist_node (** repr_cl **)
  | DomPreMerge of Age.t *
                   Cl.t * (** cl that will be equal to it
                              and from which we take the dom *)
                   domhist_node * (** domhist of this cl *)
                   domhist_node (** previous domhist *)
  | DomSet of mod_dom * domhist_node

let rec print_domhist_node fmt = function
  | DomNeverSet -> Format.pp_print_string fmt "[]"
  | DomMerge(age,l1,l2) ->
    Format.fprintf fmt "DM(%a,%a,%a)"
      Age.pp age print_domhist_node l1 print_domhist_node l2
  | DomPreMerge(age,from_cl,_,l) ->
    Format.fprintf fmt "(%a,%a,_)::%a"
      Age.pp age
      Cl.pp from_cl
      print_domhist_node l
  | DomSet(moddom,l) ->
    Format.fprintf fmt "%a::%a"
      print_mod_dom moddom
      print_domhist_node l

type domhist = domhist_node Cl.M.t Dom.Vector.t

let print_domhist fmt x =
  (Dom.Vector.pp Pp.newline Pp.colon {Dom.Vector.printk = Dom.pp}
     (Pp.iter2 Cl.M.iter Pp.semi Pp.comma Cl.pp print_domhist_node))
    fmt x

type dom_before_last_dec =
  { dom_before_last_dec: 'a. 'a dom -> Cl.t -> 'a option }

type t = {
  mutable last_dec : Age.t;
  mutable first_dec : Age.t;
  mutable nbdec    : int;
  mutable age      : Age.t;
  mutable trail    : modif list;
  mutable clgraph   : clgraph;
  mutable clhist   : clhist;
  mutable dom_before_last_dec: dom_before_last_dec;
  domhist      : domhist;
}


let create () = {
  age    = Age.bef;
  trail  = [];
  clhist = Cl.M.empty;
  clgraph = Cl.M.empty;
  domhist = Dom.Vector.create (Dom.hint_size ());
  last_dec = Age.bef;
  first_dec = max_int;
  dom_before_last_dec = {dom_before_last_dec = (fun _ _ -> None)};
  nbdec = 0;
}

let new_handler t = {
  age    = t.age;
  trail  = t.trail;
  clhist = t.clhist;
  clgraph = t.clgraph;
  domhist    = Dom.Vector.copy t.domhist;
  last_dec = t.last_dec;
  first_dec = t.first_dec;
  dom_before_last_dec = t.dom_before_last_dec;
  nbdec = t.nbdec;
}

let current_age t = t.age
let last_dec t = t.last_dec
let dom_before_last_dec t dom cl =
  t.dom_before_last_dec.dom_before_last_dec dom cl
let nbdec t = t.nbdec
let at_current_level t age = Age.compare t.last_dec age <= 0
let before_first_dec t age = Age.compare t.first_dec age > 0
let trail t = t.trail

let push (* modif *) t =
  (* t.trail <- modif::t.trail; *)
  t.age <- t.age + 1; (** t.age correspong now to the state after this
                         modification *)
  (* Format.fprintf (Debug.get_debug_formatter ()) "domhist:@[%a@]" *)
  (*   print_domhist t.domhist; *)
  Debug.dprintf2 debugage "[Trail] @[new age %a@]" Age.pp t.age


let new_dec dom_before_last_dec t  =
  t.dom_before_last_dec <- dom_before_last_dec;
  t.nbdec <- t.nbdec + 1;
  let dec = t.age + 1 in
  t.last_dec <- dec;
  if t.first_dec == max_int then t.first_dec <- dec;
  push (*Dec dec*) t;
  Debug.dprintf2 debug "[Trail] @[new dec %a@]" Age.pp dec;
  dec

let mk_pexp t ?age ?(tags=Tags.empty) kexp exp =
  (** This modification is evaluated by default in the state of the
      last regstered modification *)
  let age = match age with
    | None -> t.age
    | Some age -> assert (Age.compare age t.age <= 0); age in
  Pexp(age,kexp,exp,tags,Concache.mk ())

let mk_pexp_direct ~age ?(tags=Tags.empty) kexp exp =
  Pexp(age,kexp,exp,tags,Concache.mk ())


let add_pexp_cl t pexp ~inv ~other_cl ~other_cl0 ~repr_cl ~repr_cl0  =
  (* let modif = Cl(other_cl0,repr_cl0) in *)
  push (* modif *) t;
  Debug.dprintf10 debug
    "[Trail] @[merge %a(%a) -> %a(%a) at %a@]"
    Cl.pp other_cl0 Cl.pp other_cl
    Cl.pp repr_cl0 Cl.pp repr_cl
    Age.pp t.age (* print_modif modif *);
  (** update clgraph *)
  let add_edge cl1_0 cl2_0 inv =
    t.clgraph <- Cl.M.add_change Lists.singleton Lists.add
        cl1_0 {nage = t.age; ncl = cl2_0; npexp = pexp; ninv = inv} t.clgraph in
  add_edge other_cl0 repr_cl0 inv;
  add_edge repr_cl0  other_cl0 (not inv)

let add_merge_dom_no
    t ~inv:_ ~other_cl ~other_cl0 ~repr_cl ~repr_cl0  =
  push (* modif *) t;
  Debug.dprintf10 debug
    "[Trail] @[finalmerge without dom %a(%a) -> %a(%a) at %a@]"
    Cl.pp other_cl0 Cl.pp other_cl
    Cl.pp repr_cl0 Cl.pp repr_cl
    Age.pp t.age (* print_modif modif *);
  (** update clhist *)
  t.clhist <- Cl.M.add other_cl (t.age,repr_cl) t.clhist


let add_merge_dom_all
    t ~inv:_ ~other_cl ~other_cl0 ~repr_cl ~repr_cl0  =
  push (* modif *) t;
  Debug.dprintf10 debug
    "[Trail] @[finalmerge with dom %a(%a) -> %a(%a) at %a@]"
    Cl.pp other_cl0 Cl.pp other_cl
    Cl.pp repr_cl0 Cl.pp repr_cl
    Age.pp t.age (* print_modif modif *);
  (** update clhist *)
  t.clhist <- Cl.M.add other_cl (t.age,repr_cl) t.clhist;
  (**update domhist *)
  let apply m =
    try Cl.M.add_change
          (fun (age,o) -> DomMerge(age,o,DomNeverSet))
          (fun (age,o) r -> DomMerge(age,o,r))
          repr_cl (t.age,Cl.M.find other_cl m) m
    with Not_found -> m
  in
  Dom.Vector.apply_initialized apply t.domhist

let add_pexp_dom t pexp dom ~cl ~cl0 =
  if Dom.Vector.is_uninitialized t.domhist dom then
    Dom.Vector.set t.domhist dom Cl.M.empty;
  push t;
  Debug.dprintf6 debug "[Trail] @[add dom cl:%a cl0:%a %a@]"
    Cl.pp cl Cl.pp cl0 Age.pp t.age;
  let cm = Dom.Vector.get t.domhist dom in
  let append md m = DomSet(md,m) in
  let singleton md = append md DomNeverSet in
  let cm = Cl.M.add_change singleton append cl
      {modage=t.age;modpexp=pexp;modcl=cl0} cm in
  Dom.Vector.set t.domhist dom cm

let add_pexp_dom_premerge t dom ~clto ~clfrom ~clfrom0 =
  if Dom.Vector.is_uninitialized t.domhist dom then
    Dom.Vector.set t.domhist dom Cl.M.empty;
  push t;
  Debug.dprintf8 debug
    "[Trail] @[add premerge to_cl:%a from_cl:%a(%a) dom %a@]"
    Cl.pp clto Cl.pp clfrom0 Cl.pp clfrom Age.pp t.age;
  let cm = Dom.Vector.get t.domhist dom in
  let append (age,cl,m') m = DomPreMerge(age,cl,m',m) in
  let singleton x = append x DomNeverSet in
  let cm = Cl.M.add_change singleton append clto
      (t.age,clfrom0,Cl.M.find clfrom cm) cm in
  Dom.Vector.set t.domhist dom cm

let add_pexp_value _t _pexp _dom ~cl:_ ~cl0:_ =
  assert false (** TODO nearly all this module will change*)

(*
  let age = Cl.M.find_def Age.bef cl (Dom.Vector.get t.dom dom) in
  if age <= t.last_dec
  then (** last modified before current level *)
    begin
      push (DomL (cl,dom,v,age,pexp,cl0)) t;
      Dom.Vector.set t.dom dom (Cl.M.add cl t.age (Dom.Vector.get t.dom dom));
    end
  else (** already modified after current level *)
    push (Dom (cl,dom,pexp,cl0)) t;
  Debug.dprintf10 debug
    "[Trail] @[change dom %a of (%a)%a at %a evaluated at %a@]"
    Dom.pp dom Cl.pp cl0 Cl.pp cl Age.pp t.age pp_pexp pexp
*)

let expfact : unit exp = Exp.create_key "Explanation.fact"
let pexpfact = Pexp(Age.bef,expfact,(),Tags.empty,Concache.mk ())

type invclhist = Cl.t Age.M.t Cl.H.t

let print_invclhist fmt h =
  Pp.iter2 Cl.H.iter Pp.newline Pp.colon Cl.pp
    (Pp.iter2 Age.M.iter Pp.semi Pp.comma Age.pp Cl.pp)
    fmt h

let invclhist t =
  let invclhist = Cl.H.create 100 in
  let iter cl (age,cl') =
    (* if Age.compare t.last_dec age <= 0 then *)
      let m = Cl.H.find_def invclhist Age.M.empty cl' in
      let m = Age.M.add_new Impossible age cl m in
      Cl.H.replace invclhist cl' m
  in
  Cl.M.iter iter t.clhist;
  invclhist

type pcho =
  | Pcho: dec * ('k,'d) cho * 'k * 'd -> pcho

let expcho = Exp.create_key "Core.expcho"

let mk_pcho dec cho k d =
  mk_pexp_direct ~age:dec expcho (Pcho(dec,cho,k,d))
