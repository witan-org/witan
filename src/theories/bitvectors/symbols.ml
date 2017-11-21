open Format

type Syntax.Symbols.t +=
  | Extract of { hi:int; lo:int; length:int }
  | Conc of int*int
  | CstBV of String.t

exception Unknown

let id = function
  | Extract _ -> 0
  | Conc    _ -> 1
  | CstBV   _ -> 2
  | _ -> raise Unknown

let equal _ s1 s2 = match s1,s2 with
  | Extract{hi=hi1;lo=lo1;length=l1}, Extract{hi=hi2;lo=lo2;length=l2}
    -> hi1 = hi2 && lo1 = lo2 && l1 = l2
  | Conc(l1,l1'), Conc(l2,l2')
    -> l1 = l2 && l1' = l2'
  | CstBV l1, CstBV l2
    -> String.equal l1 l2
  | _ -> (id s1) = (id s2)

let compare _ s1 s2 = match s1,s2 with
  | Extract{hi=hi1;lo=lo1;length=l1}, Extract{hi=hi2;lo=lo2;length=l2}
    -> lex_compare (lex_compare compare compare) compare ((hi1,lo1),l1) ((hi2,lo2),l2)
  | Conc(l1,l1'), Conc(l2,l2')
    -> lex_compare compare compare (l1,l1') (l2,l2')
  | CstBV l1, CstBV l2
    -> String.compare l1 l2
  | _ -> compare (id s1) (id s2)

let arity = function
  | Extract{hi;lo;length} -> Sorts.BV (Pervasives.max (hi-lo+1) 0), [Sorts.BV length]
  | Conc(l1,l2)           -> Sorts.BV(l1+l2), [Sorts.BV l1; Sorts.BV l2]
  | CstBV s               -> Sorts.BV(String.length s), []
  | _ -> raise Unknown

let pp_latex _pp_sorts fmt = function
  | Extract{hi;lo;_} when hi = lo -> fprintf fmt "\\mbox{\\small extract}_{%i}" hi
  | Extract{hi;lo;_} -> fprintf fmt "\\mbox{\\small extract}_{%i:%i}" hi lo
  | Conc(_,_)   -> fprintf fmt "\\circl"
  | CstBV s     -> fprintf fmt "%s" s
  | _ -> raise Unknown


let pp_utf8 _pp_sorts fmt = function
  | Extract{hi;lo;_} when hi = lo -> fprintf fmt "ex[%i]" hi
  | Extract{hi;lo;_} -> fprintf fmt "ex[%i:%i]" hi lo
  | Conc(_,_)   -> fprintf fmt "⚬"
  | CstBV s     -> fprintf fmt "%s" s
  | _ -> raise Unknown

let pp = pp_utf8
