open Syntax

type Symbols.t +=
  | Ge | Le | Gt | Lt
  | Plus | Minus | Times | Divide | Op

exception Unknown

val equal   : (Sorts.t -> Sorts.t -> bool) -> Symbols.t -> Symbols.t -> bool
val compare : (Sorts.t -> Sorts.t -> int) -> Symbols.t -> Symbols.t -> int
val arity   : Symbols.t -> Symbols.arity
val pp : (Format.formatter -> Sorts.t -> unit) -> Format.formatter -> Symbols.t -> unit

