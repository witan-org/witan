(******************************)
(* Extensible type of symbols *)
(******************************)

open Format
       
type arity = Sorts.t*(Sorts.t list) [@@deriving eq, ord]

type t = ..

type t +=
  (* General *)
  | Eq of Sorts.t | NEq of Sorts.t

  (* User-declared *)
  | User of String.t*arity

  (* Prop *)
  | True | False | Neg | And | Or | Imp | Xor
  | Forall of Sorts.t | Exists of Sorts.t

exception Unknown

let id = function
  | True -> 1
  | False -> 2
  | Neg -> 3
  | And -> 4
  | Or -> 5
  | Imp -> 6
  | Xor -> 7
  | User _ -> 8
  | _ -> raise Unknown

let equal s1 s2 = match s1,s2 with
  | User(st1,a1), User(st2,a2) when String.equal st1 st2 && equal_arity a1 a2 -> true
  | _ -> (id s1) = (id s2)

let compare s1 s2 = match s1,s2 with
  | User(st1,a1), User(st2,a2) -> [%ord: String.t*arity] (st1,a1) (st2,a2)
  | _ -> compare (id s1) (id s2)

let arity = function
  | User(_,ar)                        -> ar
  | True | False                      -> Sorts.Prop, []
  | Neg | Forall _ | Exists _         -> Sorts.Prop, [Sorts.Prop]
  | And | Or | Imp | Xor              -> Sorts.Prop, [Sorts.Prop;Sorts.Prop]
  | Eq so | NEq so                    -> Sorts.Prop, [so;so]
  | _ -> raise Unknown


let pp_latex fmt = function
  | Eq so       -> fprintf fmt "=_{%a}" Sorts.pp so
  | NEq so      -> fprintf fmt "\\neq_{%a}" Sorts.pp so
  | User(f,_)   -> fprintf fmt "\\mbox{\\small %s}" f
  | True        -> fprintf fmt "\\top"
  | False       -> fprintf fmt "\\bot"
  | Neg         -> fprintf fmt "\\neg"
  | Forall s    -> fprintf fmt "\\forall^{%a}" Sorts.pp s
  | Exists s    -> fprintf fmt "\\exists^{%a}" Sorts.pp s
  | And         -> fprintf fmt "\\wedge"
  | Or          -> fprintf fmt "\\vee"
  | Imp         -> fprintf fmt "\\Rightarrow"
  | Xor         -> fprintf fmt "\\oplus"
  | _ -> raise Unknown

let pp_utf8 fmt = function
  | Eq _        -> fprintf fmt "="
  | NEq _       -> fprintf fmt "≠"
  | User(f,_)   -> fprintf fmt "%s" f
  | True        -> fprintf fmt "⊤"
  | False       -> fprintf fmt "⊥"
  | Neg         -> fprintf fmt "¬"
  | Forall _    -> fprintf fmt "∀"
  | Exists _    -> fprintf fmt "∃"
  | And         -> fprintf fmt "∧"
  | Or          -> fprintf fmt "∨"
  | Imp         -> fprintf fmt "⇒"
  | Xor         -> fprintf fmt "⊻"
  | _ -> raise Unknown

let pp = pp_utf8
let show = show_of_pp pp
