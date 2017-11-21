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

(** Module for shuffling arbitrary choices. *)

(** {1 Initialization } *)

val set_shuffle: int array option -> unit
(** if None is given shuffling is disable (default). The functions are
    the identity *)

val is_shuffle: unit -> bool

(** {1 Shuffling on common types } *)

val shuffle2: ('a * 'a) -> ('a * 'a)
(** [shuffle p] invert or keep identical the elements of the pair.
    Uniform *)

val shuffle3: ('a * 'a * 'a) -> ('a * 'a * 'a)
(** uniform *)

val shufflel: 'a list -> 'a list
(** not uniform *)

val seq2: ('a -> 'b) -> ('a * 'a) -> ('b * 'b)
(** uniform *)

val seq3: ('a -> 'b) -> ('a * 'a * 'a) -> ('b * 'b * 'b)
(** uniform *)

val seql': ('a -> unit) -> 'a list -> unit
val seql : (unit -> unit) list -> unit

val chooseb: ('a -> 'b) -> ((unit -> bool) ->'a -> 'b) -> 'a -> 'b
(** [chooseb f g] call f if there is no shuffling or g otherwise.
    The first argument given to g is a random boolean generator.
*)

val choosef: ('a -> 'b) -> ((float -> float) ->'a -> 'b) -> 'a -> 'b
val choosei: ('a -> 'b) -> ((int -> int) ->'a -> 'b) -> 'a -> 'b

val int: int -> int
