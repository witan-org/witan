open Format

type Syntax.Symbols.t +=
  | Select of {indices:Syntax.Sorts.t; values:Syntax.Sorts.t}
  | Store  of {indices:Syntax.Sorts.t; values:Syntax.Sorts.t}
  | Diff   of {indices:Syntax.Sorts.t; values:Syntax.Sorts.t}

(* BitVectors
 *   | Extract of { hi:int; lo:int; length:int }
 *   | Conc of int*int
 *   | CstBV of String.t *)

exception Unknown

let id = function
  | Select _ -> 0
  | Store  _ -> 1
  | Diff   _ -> 2
  | _ -> raise Unknown

let equal equal_sorts s1 s2 = match s1,s2 with
  | Select{indices=i1;values=v1}, Select{indices=i2;values=v2}
  | Store {indices=i1;values=v1}, Store {indices=i2;values=v2}
  | Diff  {indices=i1;values=v1}, Diff  {indices=i2;values=v2}
    -> equal_sorts i1 i2 && equal_sorts v1 v2
  | _ -> (id s1) = (id s2)

let compare compare_sorts s1 s2 = match s1,s2 with
  | Select{indices=i1;values=v1}, Select{indices=i2;values=v2}
  | Store {indices=i1;values=v1}, Store {indices=i2;values=v2}
  | Diff  {indices=i1;values=v1}, Diff  {indices=i2;values=v2}
    -> lex_compare compare_sorts compare_sorts (i1,v1) (i2,v2)
  | _ -> compare (id s1) (id s2)

let arity = function
  | Select{indices;values} -> values, [Sorts.Array{indices;values};indices]
  | Store {indices;values} -> Sorts.Array{indices;values}, [Sorts.Array{indices;values}; indices; values]
  | Diff  {indices;values} -> indices, [Sorts.Array{indices;values}; Sorts.Array{indices;values};]
  | _ -> raise Unknown

let pp_latex _pp_sorts fmt = function
  | Select _ -> fprintf fmt "\\mbox{\\small select}"
  | Store _  -> fprintf fmt "\\mbox{\\small store}"
  | Diff _   -> fprintf fmt "\\mbox{\\small diff}"
  | _ -> raise Unknown

let pp_utf8 _pp_sorts fmt = function
  | Select _ -> fprintf fmt "select"
  | Store _  -> fprintf fmt "store"
  | Diff _   -> fprintf fmt "diff"
  | _ -> raise Unknown
           
let pp = pp_utf8
