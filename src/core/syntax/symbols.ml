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
  | Eq _ -> 9
  | NEq _ -> 10
  | Forall _ -> 11
  | Exists _ -> 12
  | _ -> raise Unknown

let equal equal_sorts s1 s2 = match s1,s2 with
  | User(st1,a1), User(st2,a2) -> String.equal st1 st2 && equal_arity a1 a2
  | Eq so1, Eq so2
  | NEq so1, NEq so2
  | Forall so1, Forall so2
  | Exists so1, Exists so2
    -> equal_sorts so1 so2
  | _ -> (id s1) = (id s2)

let compare compare_sorts s1 s2 = match s1,s2 with
  | User(st1,a1), User(st2,a2) -> [%ord: String.t*arity] (st1,a1) (st2,a2)
  | Eq so1, Eq so2
  | NEq so1, NEq so2
  | Forall so1, Forall so2
  | Exists so1, Exists so2
    -> compare_sorts so1 so2
  | _ -> compare (id s1) (id s2)

let arity = function
  | User(_,ar)                 -> ar
  | True | False               -> Sorts.Prop, []
  | Neg | Forall _ | Exists _  -> Sorts.Prop, [Sorts.Prop]
  | And | Or | Imp | Xor       -> Sorts.Prop, [Sorts.Prop;Sorts.Prop]
  | Eq so | NEq so             -> Sorts.Prop, [so;so]
  | _ -> raise Unknown


let pp_latex _pp_sorts fmt = function
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

let pp_utf8 _pp_sorts fmt = function
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