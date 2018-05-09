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

type context = {
  mutable bps: bp list;
}

and bp = {
  mutable alive : bool; (** not poped *)
  context : context;
}

let bp_equal (a:bp) b = CCEqual.physical a b

let create () =
  let rec context = { bps = [bp];}
  and bp = { alive = true; context} in
  context

let bp t = match t.bps with
  | [] -> assert false (** absurd: the level0 can't be removed since there is no bp under it *)
  | bp::_ -> bp

let push context =
  let bp = {alive = true; context} in
  context.bps <- bp::context.bps

exception AlreadyPoped

let pop bp =
  if not bp.alive then raise AlreadyPoped;
  let rec aux = function
    | [] -> assert false (** absurd: by invariant bp must be in the list *)
    | (a::_) as l when bp_equal a bp ->
      bp.context.bps <- l
    | a::l ->
      assert (a.alive);
      a.alive <- false;
      aux l
  in
  aux bp.context.bps

module Ref = struct
  type 'a t = {
    mutable contents : 'a;
    mutable previous : 'a hist_bp_ref list;
    context : context;
  }

  and 'a hist_bp_ref = {
    value : 'a;
    at: bp;
  }


  let create context x = {
    contents = x;
    previous = [];
    context;
  }

  let rewind r =
    match r.previous with
    | [] -> ()
    | {at}::_ when at.alive -> ()
    | _ ->
      let rec aux v = function
        | {at;value}::l when not at.alive -> aux value l
        | l -> r.contents <- v; r.previous <- l
      in
      aux r.contents r.previous

  let set r v =
    rewind r;
    if not (CCEqual.physical r.contents v)
    then
      match r.previous with
      | {at}::_ when bp_equal at (bp r.context) -> r.contents <- v
      | _ ->
      r.previous <- {at=bp r.context; value = r.contents}::r.previous;
      r.contents <- v

  let get r =
    rewind r;
    r.contents
end

type 'a history = {
  mutable previous : 'a hist list;
  context : context;
}

and 'a hist = {
  saved : 'a;
  at: bp;
}

module Make(S:sig
    type t
    type saved

    val save: t -> saved
    val restore: saved -> t -> unit
    val get_history: t -> saved history
  end) = struct


  let create context = {
    previous = [];
    context;
  }

  let refresh t =
    let h = S.get_history t in
    match h.previous with
    | [] -> ()
    | {at}::_ when at.alive -> ()
    | {saved}::l ->
      let rec aux saved = function
        | {at;saved}::l when not at.alive -> aux saved l
        | l -> S.restore saved t; h.previous <- l
      in
      aux saved l

  let save t =
    refresh t;
    let h = S.get_history t in
    match h.previous with
    | {at}::_ when bp_equal at (bp h.context) -> ()
    | _ ->
      h.previous <- {at=bp h.context; saved = S.save t}::h.previous

  type hidden = S.t
  let ro t = refresh t; t
  let rw t = save t; t
  let hide t = t
end
