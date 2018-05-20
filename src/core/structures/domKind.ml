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

open Witan_popop_lib
open Nodes

let debug = Debug.register_info_flag
  ~desc:"for the domains"
  "Egraph.dom"

module Dom = Keys.Make_key(struct end)
include Dom
type 'a dom = 'a t

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
  val key: t Dom.t
end

module Make(S:sig type delayed type pexp end) = struct

  module type Dom = Dom_partial with type delayed := S.delayed and type pexp := S.pexp

  include Dom.Make_Registry(struct
      type 'a data = (module Dom with type t = 'a)
      let pp (type a) (dom: a data) =
        let module Dom = (val dom) in
        Dom.pp
      let key (type a) (dom: a data) =
        let module Dom = (val dom) in
        Dom.key
    end)

  let register_dom = register
  let get_dom = get
  let print_dom = print

  let print_dom_opt k fmt = function
    | None -> Format.pp_print_string fmt "N"
    | Some s -> print_dom k fmt s
end
