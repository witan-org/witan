open Format
open Witan_core

type Sorts.t += Array of {indices:Sorts.t; values:Sorts.t}

exception Unknown

let equal equal_rec s1 s2 =
  match s1, s2 with
  | Array{indices=i1;values=v1}, Array{indices=i2;values=v2}
    -> equal_rec i1 i2 && equal_rec v1 v2
  | _ -> raise Unknown

let compare compare_rec s1 s2 =
  match s1, s2 with
  | Array{indices=i1;values=v1}, Array{indices=i2;values=v2}
    -> lex_compare compare_rec compare_rec (i1,v1) (i2,v2)
  | _ -> raise Unknown

let pp pp_rec fmt = function
  | Array{indices;values} -> fprintf fmt "{\\mathbb Ar}(%a,%a)" pp_rec indices pp_rec values
  | _ -> raise Unknown
