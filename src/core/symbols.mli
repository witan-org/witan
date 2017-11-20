(***********************************************)
(* This is the collection of all known symbols *)
(***********************************************)

type arity = Sorts.t*(Sorts.t list) [@@deriving eq]

type t = .. [@@deriving eq, show, ord]

type t +=
  (* General *)
  | Eq of Sorts.t | NEq of Sorts.t

  (* Prop *)
  | True | False | Neg | And | Or | Imp | Xor
  | Forall of Sorts.t | Exists of Sorts.t

  | User of string*arity

exception Unknown

val arity : t -> arity
