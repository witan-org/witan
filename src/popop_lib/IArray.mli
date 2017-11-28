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


(** immutable arrays, like arrays but you can't modify them after
    creation *)

type 'a t

val of_list: 'a list -> 'a t
val of_array: 'a array -> 'a t
val of_iter: int -> (('a -> unit) -> unit) -> 'a t
(** create the array using an iterator. The integer indicate the
    number of iteration that will occur *)

val length: 'a t -> int

val compare: ('a -> 'a ->  int)  -> 'a t -> 'a t -> int
val equal  : ('a -> 'a -> bool)  -> 'a t -> 'a t -> bool

val get : 'a t -> int -> 'a

val hash   : ('a -> int) -> 'a t -> int


val iter : ('a -> unit)  -> 'a t -> unit
val iteri : (int -> 'a -> unit)  -> 'a t -> unit
val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val foldi : (int -> 'b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val pp: unit Pp.pp -> 'a Pp.pp -> 'a t Pp.pp
