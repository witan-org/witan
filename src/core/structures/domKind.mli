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

(** Domains *)

include Keys.Key

type 'a dom = 'a t

(** delayed and pexp are instanciated in Egraph *)
module type Dom_partial = sig
  type delayed
  type pexp
  type t

  val merged: t option -> t option -> bool
  val merge: delayed ->
    pexp -> t option * Node.t -> t option * Node.t ->
    bool ->
    unit
  val pp: Format.formatter  -> t  -> unit
  val key: t dom
end

module Make (S:sig type delayed type pexp end) : sig

  module type Dom = Dom_partial with type delayed := S.delayed and type pexp := S.pexp

  val register_dom: (module Dom with type t = 'a) -> unit
  val check_is_registered : 'a dom -> unit
  val is_well_initialized : unit -> bool
  val get_dom : 'a dom -> (module Dom with type t = 'a)
  val print_dom : 'a dom -> Format.formatter -> 'a -> unit
  val print_dom_opt : 'a dom -> Format.formatter -> 'a option -> unit

end
