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

type creator = context

let creator t = t

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

  let creator (h:'a t) = h.context

end


module Ref2 = struct
  type ('a,'b) t = {
    mutable contents1 : 'a;
    mutable contents2 : 'b;
    mutable previous : ('a,'b) history list;
    context : context;
  }

  and ('a,'b) history = {
    value1 : 'a;
    value2 : 'b;
    at: bp;
  }

  let creator (h:('a,'b) t) = h.context

  let create context x1 x2 = {
    contents1 = x1;
    contents2 = x2;
    previous = [];
    context;
  }

  let rewind r =
    match r.previous with
    | [] -> ()
    | {at}::_ when at.alive -> ()
    | _ ->
      let rec aux v1 v2 = function
        | {at;value1;value2}::l when not at.alive -> aux value1 value2 l
        | l -> r.contents1 <- v1; r.contents2 <- v2; r.previous <- l
      in
      aux r.contents1 r.contents2 r.previous

  let set1 r v1 =
    rewind r;
    if not (CCEqual.physical r.contents1 v1)
    then
      match r.previous with
      | {at}::_ when bp_equal at (bp r.context) -> r.contents1 <- v1
      | _ ->
      r.previous <- {at=bp r.context; value1 = r.contents1; value2 = r.contents2}::r.previous;
      r.contents1 <- v1

  let get1 r =
    rewind r;
    r.contents1

  let set2 r v2 =
    rewind r;
    if not (CCEqual.physical r.contents2 v2)
    then
      match r.previous with
      | {at}::_ when bp_equal at (bp r.context) -> r.contents2 <- v2
      | _ ->
      r.previous <- {at=bp r.context; value1 = r.contents1; value2 = r.contents2}::r.previous;
      r.contents2 <- v2

  let get2 r =
    rewind r;
    r.contents2

  let set r v1 v2 =
    rewind r;
    if not (CCEqual.physical r.contents1 v1 && CCEqual.physical r.contents2 v2)
    then
      match r.previous with
      | {at}::_ when bp_equal at (bp r.context) ->
        r.contents1 <- v1; r.contents2 <- v2
      | _ ->
      r.previous <- {at=bp r.context; value1 = r.contents1; value2 = r.contents2}::r.previous;
      r.contents1 <- v1;
      r.contents2 <- v2

  let get r =
    rewind r;
    r.contents1, r.contents2
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

  let creator (h:'a history) = h.context

end
