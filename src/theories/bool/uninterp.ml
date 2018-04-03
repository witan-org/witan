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

    (* let norm d s = *)
    (*   Delayed.propagate d (Delayed.index d sem s) *)

    (* let propagate ~propagate s = *)
    (*   match s with *)
    (*   | App(f,g) -> propagate f; propagate g *)

end

let pp = Th.pp

module ThE = Sem.Register(Th)

let app f g ty = Node.index_sem sem (App(f,g)) ty

let appl f l ty =
  let rec aux = function
    | [] -> assert false
    | [a] -> a
    | a::l -> app a (aux l) partty in
  let l = aux l in
  app f l ty

let result_of_sort = Ty.H.create 20

let register_sort ?dec ty =
  Ty.H.add result_of_sort ty dec

(** Bool can't register itself it is linked before uninterp *)
let () = register_sort Bool.ty ~dec:Bool.make_dec

let result_to_dec = Node.H.create 20

let fresh_fun ~result ~arity name =
  assert (arity > 0);
  let node = Node.fresh name funty in
  Node.H.add result_to_dec node (result,arity);
  node

let app_fun node args =
  let (sort,arity) = Node.H.find_exn result_to_dec Impossible node in
  assert (List.length args = arity);
  let appnode = appl node args sort in
  appnode

let fun1 ty s =
  let f = fresh_fun ~result:ty ~arity:1 s in
  (fun x -> app_fun f [x])

let fun2 ty s =
  let f = fresh_fun ~result:ty ~arity:2 s in
  (fun x1 x2 -> app_fun f [x1;x2])

let fun3 ty s =
  let f = fresh_fun ~result:ty ~arity:3 s in
  (fun x1 x2 x3 -> app_fun f [x1;x2;x3])

let fun4 ty s =
  let f = fresh_fun ~result:ty ~arity:4 s in
  (fun x1 x2 x3 x4 -> app_fun f [x1;x2;x3;x4])

let fun5 ty s =
  let f = fresh_fun ~result:ty ~arity:5 s in
  (fun x1 x2 x3 x4 x5 -> app_fun f [x1;x2;x3;x4;x5])


type expsubst = ThE.t
let expsubst : expsubst Explanation.exp =
  Explanation.Exp.create_key "Uninterp"

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
      let pexp = Delayed.mk_pexp d expsubst nodesem in
      let nodesem' = ThE.index v' (ThE.ty nodesem) in
      Delayed.set_sem d pexp (ThE.node nodesem) (ThE.nodesem nodesem');
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
          let nodesem = ThE.coerce_nodesem nodesem in
          let v = ThE.sem nodesem in
          let own = ThE.node nodesem in
          Debug.dprintf2 debug "[Uninterp] @[init %a@]" Th.pp v;
          let dec = Ty.H.find_def result_of_sort None (Node.ty own) in
          Opt.iter (fun dec -> Variable.add_dec ~dec d own) dec;
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
              let pexp = Delayed.mk_pexp d expsubst nodesem in
              let nodesem = ThE.nodesem (ThE.index v' (Node.ty own)) in
              Delayed.set_sem d pexp own nodesem
        end
      | _ -> raise UnwaitedEvent
      ) ev;
    Demon.AliveReattached


end

module RDaemonInit = Demon.Key.Register(DaemonInit)

let th_register env =
  RDaemonPropa.init env;
  RDaemonInit.init env;
  Demon.Key.attach env
    DaemonInit.key () [Demon.Create.EventRegSem(sem,())];

module ExpSubst = struct
  open Conflict
  (* open IterExp *)
  open ComputeConflict

  type t = expsubst

  let pp fmt nodesem =
    match ThE.sem nodesem with
    | App(f,g) ->
      Format.fprintf fmt "Subst(%a,%a)" Node.pp f Node.pp g

(*
  let iterexp t age = function
    | App(f,g) ->
      need_sem t age sem (App(f,g));
      need_node_repr t age f;
      need_node_repr t age g
*)
  let analyse :
    type a. Conflict.ComputeConflict.t ->
    Explanation.age -> a Explanation.con -> t -> a Conflict.rescon =
    fun t age con nodesem ->
      match ThE.sem nodesem with
      | App(f,g) ->
        let f' = get_repr_at t age f in
        let g' = get_repr_at t age g in
        let v' = App(f',g') in
        let to_ = ThE.index v' (ThE.ty nodesem) in
        Conflict.return con consubst { from=nodesem; to_ }

  let expdomlimit _ _ _ _ _ _ _ = raise Impossible (** don't propa dom *)

  let key = expsubst

  let same_sem t age _sem _v con exp _node1 _node2 =
    analyse  t age con exp

end

let () = Conflict.register_exp(module ExpSubst)
