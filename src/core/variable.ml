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

open Stdlib
open Typedef

type make_dec = Node.t -> Explanation.chogen

module Dem = struct

  module Data = struct
    type t = make_dec
    let pp fmt _ = Format.fprintf fmt "make_dec"
  end

  let immediate = false
  let key = Demon.Fast.create "Variable.dec"
  let throttle = 100
  let wakeup d = function
    | Events.Fired.EventRegNode (node,make_dec) ->
      Solver.Delayed.register_decision d (make_dec node)
    | _ -> assert false
end

module EDem = Demon.Fast.Register(Dem)

let dec_of_sort = Ty.H.create 20

let fresh ty s =
  match Ty.H.find_opt dec_of_sort ty with
  | Some make_dec ->
    Demon.Fast.fresh_with_reg_node Dem.key s ty make_dec
  | None -> Node.fresh s ty

let cst =
  let h = DStr.H.create 10 in
  fun ty s ->
    try
      let node = DStr.H.find h s in
      assert (Ty.equal (Node.ty node) ty);
      node
    with Not_found ->
      let node = fresh ty s in
      DStr.H.add h s node;
      node

let register_sort ~dec ty =
  Ty.H.add_new Std.Impossible dec_of_sort ty dec

let add_dec ~dec t node =
  Demon.Fast.attach t Dem.key
    [Demon.Create.EventRegCl(node,dec)]


let th_register env =
  EDem.init env
