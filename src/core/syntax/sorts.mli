(****************************)
(* Extensible type of sorts *)
(****************************)

type t = .. [@@deriving eq, show, ord]
type t +=
  | Prop
  | User of String.t
(*          | Fun  of t*(t list)
 *          | BV of int  *)
