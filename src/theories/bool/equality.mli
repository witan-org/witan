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

val equality    : Node.t list -> Node.t
val disequality : Node.t list -> Node.t

val is_equal    : Egraph.t -> Node.t -> Node.t -> bool
val is_disequal : Egraph.t -> Node.t -> Node.t -> bool

val ite : Node.t -> Node.t -> Node.t -> Node.t

val iter_on_value_different:
  (module ValueKind.Registered with type s = 'a and type t = 'b) ->
  they_are_different:(Trail.Pexp.t -> Node.t -> 'a -> unit) ->
  Egraph.t -> Node.t -> unit


val th_register : Egraph.t -> unit
