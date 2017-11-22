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

open Typedef

let debug = Debug.register_info_flag
  ~desc:"for the domains"
  "Solver.dom"

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

  module VDom = Dom.MkVector
      (struct type ('a,'unedeed) t =
                (module Dom with type t = 'a)
      end)

  let defined_dom : unit VDom.t = VDom.create 8

  module RegisterDom (D:Dom) = struct

    let () =
      VDom.inc_size D.key defined_dom;
      assert (if not (VDom.is_uninitialized defined_dom D.key)
              then raise AlreadyRegisteredKey else true);
      let dom = (module D: Dom with type t = D.t) in
      VDom.set defined_dom D.key dom

  end

  let check_is_registered dom =
    assert (if VDom.is_uninitialized defined_dom dom
            then raise UnregisteredKey else true)

  let well_initialized () =
    let well_initialized = ref true in
    Dom.iter {Dom.iter = fun dom ->
        if VDom.is_uninitialized defined_dom dom then begin
          Format.eprintf
            "[Warning] The domain %a is not registered" Dom.pp dom;
          well_initialized := false;
        end else begin
          Debug.dprintf2 debug "[Solver] @[domain %a initialized@]"
            Dom.pp dom;
        end};
    !well_initialized

  let is_registered dom = VDom.is_uninitialized defined_dom dom

  let get_dom k =
    assert (if VDom.is_uninitialized defined_dom k
            then raise UnregisteredKey else true);
    VDom.get defined_dom k

  let get_dom_with_prefix_to_remove = get_dom

  let print_dom (type a) (k : a dom) fmt s =
    let dom = get_dom k in
    let module D = (val dom : Dom with type t = a) in
    D.pp fmt s

  let print_dom_opt k fmt = function
    | None -> Format.pp_print_string fmt "N"
    | Some s -> print_dom k fmt s
end
