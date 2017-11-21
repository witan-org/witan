open Syntax

type Symbols.t +=
  | Extract of { hi:int; lo:int; length:int }
  | Conc of int*int
  | CstBV of String.t

exception Unknown

val equal   : (Sorts.t -> Sorts.t -> bool) -> Symbols.t -> Symbols.t -> bool
val compare : (Sorts.t -> Sorts.t -> int) -> Symbols.t -> Symbols.t -> int
val arity   : Symbols.t -> Symbols.arity
val pp : Format.formatter -> Symbols.t -> unit

