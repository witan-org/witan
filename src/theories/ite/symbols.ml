open Format
open Syntax

type Symbols.t += ITE of Sorts.t

exception Unknown

let equal equal_sorts s1 s2 = match s1,s2 with
  | ITE so1, ITE so2 -> equal_sorts so1 so2
  | _ -> raise Unknown

let compare compare_sorts s1 s2 = match s1,s2 with
  | ITE so1, ITE so2 -> compare_sorts so1 so2
  | _ -> raise Unknown

let arity = function
  | ITE so -> so, [Syntax.Sorts.Prop; so; so]
  | _ -> raise Unknown

let pp_latex fmt = function
  | ITE _ -> fprintf fmt "\\mbox{if}"
  | _     -> raise Unknown

let pp_utf8 fmt = function
  | ITE _  -> fprintf fmt "if"
  | _      -> raise Unknown

let pp = pp_utf8
