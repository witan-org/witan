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

open Witan_core

type t = App of Node.t * Node.t
val pp: t Format.printer

val sem: t ThTermKind.t

(* val fun1 :
 *   Ty.t -> string ->
 *   (Node.t -> Node.t)
 * val fun2 :
 *   Ty.t -> string ->
 *   (Node.t -> Node.t -> Node.t)
 * val fun3 :
 *   Ty.t -> string ->
 *   (Node.t -> Node.t -> Node.t -> Node.t)
 * val fun4 :
 *   Ty.t -> string ->
 *   (Node.t -> Node.t -> Node.t -> Node.t -> Node.t)
 * val fun5 :
 *   Ty.t -> string ->
 *   (Node.t -> Node.t -> Node.t -> Node.t -> Node.t -> Node.t)
 * 
 * val fresh_fun: result:Ty.t -> arity:int -> string -> Node.t *)

(* val app_fun: Node.t -> Node.t list -> Node.t *)

val th_register : Egraph.t -> unit
