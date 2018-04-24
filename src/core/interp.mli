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


module Register: sig
  val id: (Term.id -> Nodes.Value.t list -> Nodes.Value.t option) -> unit

  val thterm: 'a Nodes.ThTermKind.t -> (interp:(Nodes.Node.t -> Nodes.Value.t) -> 'a -> Nodes.Value.t) -> unit

  val model: Ty.t -> (Egraph.t -> Nodes.Node.t -> Nodes.Value.t) -> unit

end

type leaf = Term.t -> Nodes.Value.t option

val term   : ?leaf:leaf -> Term.t -> Nodes.Value.t
val thterm : ?leaf:leaf -> Nodes.ThTerm.t -> Nodes.Value.t
val node   : ?leaf:leaf -> Nodes.Node.t -> Nodes.Value.t

val model : Egraph.t -> Nodes.Node.t -> Nodes.Value.t
