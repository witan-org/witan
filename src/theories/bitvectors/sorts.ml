open Format
open Syntax

type Sorts.t += BV of int

exception Unknown

let equal _ s1 s2 =
  match s1, s2 with
  | BV i, BV j -> i = j
  | _ -> raise Unknown

let compare _ s1 s2 =
  match s1, s2 with
  | BV i, BV j -> compare i j
  | _ -> raise Unknown

let pp _ fmt = function
  | BV i -> fprintf fmt "\"BV_{%i}\"" i
  | _ -> raise Unknown
