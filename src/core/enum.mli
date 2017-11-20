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

(** Use enum for deforesting *)

type 'a t

val for_all: ('a -> bool) -> 'a t -> bool
val exists: ('a -> bool) -> 'a t -> bool
val is_empty: 'a t -> bool

val fold: ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val iter: ('a -> unit) -> 'a t -> unit

val list_rev: 'a t -> 'a list
(** return the enumeration in the reverse order *)

(** Create Enum *)
val from_list: ?filter:('b -> bool) -> map:('b -> 'a) -> 'b list  -> 'a t
val from_bag : ?filter:('b -> bool) -> map:('b -> 'a) -> 'b Bag.t -> 'a t
