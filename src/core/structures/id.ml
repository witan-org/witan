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

(* Generic Identifiers *)
(* ************************************************************************ *)

type 'a t = {
  index   : int;     (** unique index *)
  name    : string;  (** id name *)
  ty      : 'a;      (** Identifier type *)
} [@@deriving eq]
(** Polymorphic type for identifiers. Each identifiers has a unique index
    used for fast comparisons. Additionally, each id has a type. *)

type 'a id = 'a t [@@deriving eq]
(** Alias necessary because of recursive types by default (see type definition
    for Any.t). *)

(** Usual functions *)
let hash v = v.index (* TODO: hash the index ? *)
let compare v v' = Pervasives.compare v.index v'.index
let equal v v' = compare v v' = 0

let print fmt { name; _ } =
  Format.fprintf fmt "%s" name
let pp = print

(** Create new identifiers.
    Each id is fresh and unique, thus ids that must be used in multiples
    places should be remembered, either in a map for scoping, or exposed in module
    interface. *)
let id_counter = ref 0
let mk name ty = incr id_counter; { index = !id_counter; name; ty; }

(** Accessor *)
let ty { ty; _ } = ty
let name { name; _ } = name

(* Wrapper around Any identifiers *)
(* ************************************************************************ *)

module Any = struct

  (** Wrapper to be able to store arbitrary ids in maps, hashtbl, etc... *)
  type t = Any : 'a id -> t [@@unboxed]

  let hash (Any t) = hash t
  let compare (Any t) (Any t') = compare t t'
  let equal any any' = compare any any' = 0
  let print fmt (Any t) = print fmt t

  let mk id = Any id

end
