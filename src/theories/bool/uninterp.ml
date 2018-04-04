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
open Egraph

let debug = Debug.register_info_flag
  ~desc:"for the uninterpreted function theory"
  "uninterp"

type t = App of Node.t * Node.t

let sem : t Sem.t = Sem.create_key "UF"

module Th = struct
  module T = struct
    type r = t
    type t = r
    let equal n m =
      match n, m with
      | App (g1,f1), App(g2,f2) -> Node.equal g1 g2 && Node.equal f1 f2

    let hash n =
      match n with
      | App(g,f) -> 3 * (Node.hash g) + 5 * (Node.hash f)

    let compare n m =
      match n, m with
      | App(g1,f1), App(g2,f2) ->
        let c = Node.compare g1 g2 in
        if c <> 0 then c
        else Node.compare f1 f2

    let pp fmt v =
      match v with
      | App (f,g) -> Format.fprintf fmt "(@[%a@],@,@[%a@])"
        Node.pp  f Node.pp g

  end

  include T
  include MkDatatype(T)

  let key = sem

end

let pp = Th.pp

module ThE = Sem.Register(Th)

let app f g ty = Node.index_sem sem (App(f,g)) ty

let appl f l =
  let rec aux acc ty = function
    | [] -> acc
    | a::l ->
      let _,ty = Term.extract_fun_ty ty in
      aux (app acc a ty) ty l
  in
  aux f (Node.ty f) l

(* let result_of_sort = Ty.H.create 20
 * 
 * let register_sort ?dec ty =
 *   Ty.H.add result_of_sort ty dec
 * 
 * (\** Bool can't register itself it is linked before uninterp *\)
 * let () = register_sort Bool.ty ~dec:Bool.make_dec *)

(* let result_to_dec = Node.H.create 20
 * 
 * let app_fun node args =
 *   let (sort,arity) = Node.H.find_exn result_to_dec Impossible node in
 *   assert (List.length args = arity);
 *   let appnode = appl node args sort in
 *   appnode
 * 
 * let fun1 ty s =
 *   let f = fresh_fun ~result:ty ~arity:1 s in
 *   (fun x -> app_fun f [x])
 * 
 * let fun2 ty s =
 *   let f = fresh_fun ~result:ty ~arity:2 s in
 *   (fun x1 x2 -> app_fun f [x1;x2])
 * 
 * let fun3 ty s =
 *   let f = fresh_fun ~result:ty ~arity:3 s in
 *   (fun x1 x2 x3 -> app_fun f [x1;x2;x3])
 * 
 * let fun4 ty s =
 *   let f = fresh_fun ~result:ty ~arity:4 s in
 *   (fun x1 x2 x3 x4 -> app_fun f [x1;x2;x3;x4])
 * 
 * let fun5 ty s =
 *   let f = fresh_fun ~result:ty ~arity:5 s in
 *   (fun x1 x2 x3 x4 x5 -> app_fun f [x1;x2;x3;x4;x5]) *)


type expsubst = {from:ThE.t; to_:ThE.t}
let expsubst : expsubst Trail.Exp.t =
  Trail.Exp.create_key "Uninterp"

module DaemonPropa = struct
  type k = ThE.t
  let key = Demon.Key.create "Uninterp.DaemonPropa"

  module Key = ThE
  module Data = DUnit
  type info = unit let default = ()

  let is_unborn d v =
    let open Demon.Key in
    match is_attached d key v with
    | SDead | SRedirected _ | SAlive () -> false
    | SUnborn -> true

  let attach d f g v =
    let open Demon.Create in
    assert (is_unborn d v);
    Demon.Key.attach d key v
      [EventChange(f,()); EventChange(g,())]

  let immediate = true (** can be false *)
  let wakeup d (nodesem:k) _ev () =
    match ThE.sem nodesem with
    | App(f,g) as v ->
      Debug.dprintf4 debug "[Uninterp] @[wakeup own %a v:%a@]"
        Node.pp (ThE.node nodesem) Th.pp v;
      let v' = App(Delayed.find d f, Delayed.find d g) in
      assert (not (Th.equal v v'));
      let nodesem' = ThE.index v' (ThE.ty nodesem) in
      let pexp = Delayed.mk_pexp d expsubst {from=nodesem;to_=nodesem'} in
      Delayed.set_sem d pexp (ThE.node nodesem) (ThE.thterm nodesem');
      Demon.AliveRedirected nodesem'
end

module RDaemonPropa = Demon.Key.Register(DaemonPropa)

module DaemonInit = struct
  let key = Demon.Key.create "Uninterp.DaemonInit"

  module Key = DUnit
  module Data = DUnit
  type info = unit let default = ()

  let immediate = false

  let wakeup d () ev () =
    List.iter
      (function Events.Fired.EventRegSem(nodesem,()) ->
        begin
          let nodesem = ThE.coerce_thterm nodesem in
          let v = ThE.sem nodesem in
          let own = ThE.node nodesem in
          Debug.dprintf2 debug "[Uninterp] @[init %a@]" Th.pp v;
          if DaemonPropa.is_unborn d nodesem then
          match v with
          | App(f,g) ->
            Delayed.register d f; Delayed.register d g;
            let f' = Delayed.find d f in
            let g' = Delayed.find d g in
            if Node.equal f' f && Node.equal g' g then
              DaemonPropa.attach d f g nodesem
            else
              let v' = App(f',g') in
              let nodesem' = ThE.index v' (Node.ty own) in
              let pexp = Delayed.mk_pexp d expsubst {from=nodesem;to_=nodesem'} in
              Delayed.set_sem d pexp own (ThE.thterm nodesem')
        end
      | _ -> raise UnwaitedEvent
      ) ev;
    Demon.AliveReattached


end

module RDaemonInit = Demon.Key.Register(DaemonInit)

module ExpSubst = struct
  open Conflict

  type t = expsubst
  let key = expsubst

  let pp fmt c =
    match ThE.sem c.from, ThE.sem c.to_ with
    | App(f,g), App(f',g') ->
      Format.fprintf fmt "Subst(%a,%a -> %a,%a)"
        Node.pp f Node.pp g Node.pp f' Node.pp g'

  let analyse t {from;to_} pcon =
    match ThE.sem from, ThE.sem to_ with
    | App(f,g), App(f',g') ->
      let lcon = Conflict.split t pcon (ThE.node from) (ThE.node to_) in
      let lcon' = if Node.equal f f' then [] else EqCon.create_eq f f' in
      let lcon'' = if Node.equal g g' then [] else EqCon.create_eq g g' in
      lcon'@lcon''@lcon

  let from_contradiction _ _ = raise Impossible

end

let () = Conflict.register_exp(module ExpSubst)


let converter d f l =
  let of_term t =
    let n = SynTerm.node_of_term t in
    Egraph.Delayed.register d n;
    n
  in
  let node = match f with
    | {Term.term=Id id} when not (Term.is_defined id) ->
      let f = of_term f in
      let l = List.map of_term l in
      Some (appl f l)
    | _ -> None
  in
      node

let th_register env =
  RDaemonPropa.init env;
  RDaemonInit.init env;
  Demon.Key.attach env
    DaemonInit.key () [Demon.Create.EventRegSem(sem,())];
  SynTerm.register_converter env converter;
  ()
