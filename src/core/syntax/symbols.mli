(***********************************************)
(* This is the collection of all known symbols *)
(***********************************************)

type arity = Sorts.t*(Sorts.t list) [@@deriving eq]

type t = ..

type t +=
  (* General *)
  | Eq of Sorts.t | NEq of Sorts.t

  (* Prop *)
  | True | False | Neg | And | Or | Imp | Xor
  | Forall of Sorts.t | Exists of Sorts.t

  | User of string*arity

exception Unknown

val equal   : (Sorts.t -> Sorts.t -> bool) -> t -> t -> bool
val compare : (Sorts.t -> Sorts.t -> int) -> t -> t -> int
val arity : t -> arity
val pp : (Format.formatter -> Sorts.t -> unit) -> Format.formatter -> t -> unit
