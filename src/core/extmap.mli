(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* This file originates from the OCaml v 3.12 Standard Library.
   It was extended and modified for the needs of the Why3 project.
   It is distributed under the terms of its initial license, which
   is provided in the file OCAML-LICENSE. *)

(** Association tables over ordered types

   This module implements applicative association tables, also known as
   finite maps or dictionaries, given a total ordering function
   over the keys.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map.
*)

module type S = sig
  include Map_intf.Map with type 'a data = 'a

  type 'a view =
    | Empty
    | Node of 'a view * key * 'a * 'a view * int

  val view: 'a t -> 'a view

end

module Make (Ord : Map_intf.OrderedType) : S with type key = Ord.t
(** Functor building an implementation of the map structure
    given a totally ordered type. *)
