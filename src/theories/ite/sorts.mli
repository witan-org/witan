open Syntax

exception Unknown
  
val equal   : _ -> Sorts.t -> Sorts.t -> bool
val compare : _ -> Sorts.t -> Sorts.t -> int
val pp : _ -> Format.formatter -> Sorts.t -> unit
