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

module type S = sig

  module Sorts : sig
    exception Unknown
    open Syntax
    val equal   : (Sorts.t -> Sorts.t -> bool) -> Sorts.t -> Sorts.t -> bool
    val compare : (Sorts.t -> Sorts.t -> int) -> Sorts.t -> Sorts.t -> int
    val pp : (Format.formatter -> Sorts.t -> unit) -> Format.formatter -> Sorts.t -> unit
  end

  module Symbols : sig
    exception Unknown
    open Syntax
    val equal   : (Sorts.t -> Sorts.t -> bool) -> Symbols.t -> Symbols.t -> bool
    val compare : (Sorts.t -> Sorts.t -> int) -> Symbols.t -> Symbols.t -> int
    val arity   : Symbols.t -> Symbols.arity
    val pp : (Format.formatter -> Sorts.t -> unit) -> Format.formatter -> Symbols.t -> unit
  end

  module SemanticTerms : sig
    type t [@@deriving eq, ord]
    val build: Syntax.Symbols.t -> t list -> t (* Interpretation of symbols *)
  end  
  
end
