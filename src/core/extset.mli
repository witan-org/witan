(***********************************************************************)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*    en Automatique.                                                  *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Lesser General Public License version 2.1, with the        *)
(*  special exception on linking described in the file LICENSE.        *)
(***********************************************************************)

(** Sets over ordered types *)

module type S = Map_intf.Set


module MakeOfMap (M : Map_intf.MapUnit) : S with type 'a M.t = 'a M.t
                                             and type M.key = M.key
(** Functor building an implementation of the set structure
    given a totally ordered type. *)

module Make (Ord : Map_intf.OrderedType) : S with type M.key = Ord.t
(** Functor building an implementation of the set structure
    given a totally ordered type. *)

module MakeHashcons(MH:Map_intf.Map_hashcons with type 'a data = unit):
  Map_intf.Set_hashcons with type 'a poly = 'a MH.poly
                         and type M.key = MH.key
(** Functor building an implementation of the hasconsed set structure
    given a totally ordered hashconsed map. *)
