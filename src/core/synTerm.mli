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

open Nodes

val key: Term.t ThTermKind.t

include RegisteredThTerm with type s = Term.t

val node_of_term : Term.t -> Node.t

val init: Egraph.Delayed.t -> unit

val register_converter:
  Egraph.Delayed.t ->
  (Egraph.Delayed.t -> Term.t -> Term.t list -> Node.t option) ->
  unit
(** register converters between syntactic terms *)


val register_decvars:
  Egraph.Delayed.t ->
  (Node.t -> Trail.chogen option) ->
  unit
(** register decision adder on variables *)
