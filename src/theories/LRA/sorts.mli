(*****************************************)
(* This is the collection of known sorts *)
(*****************************************)

open Syntax

type Sorts.t += Rat

val equal   : (Sorts.t -> Sorts.t -> bool) -> Sorts.t -> Sorts.t -> bool
val compare : (Sorts.t -> Sorts.t -> int) ->  Sorts.t -> Sorts.t -> int
val pp : (Format.formatter -> Sorts.t -> unit) -> Format.formatter -> Sorts.t -> unit
