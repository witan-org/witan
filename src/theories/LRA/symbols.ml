open Format
    
type Syntax.Symbols.t +=
  | CstRat of Q.t
  | Ge | Le | Gt | Lt
  | Plus | Minus | Times | Divide | Op

exception Unknown

let id = function
  | CstRat _ -> 0
  | Ge -> 1
  | Le -> 2
  | Gt -> 3
  | Lt -> 4
  | Plus -> 5
  | Minus -> 6
  | Times -> 7
  | Divide -> 8
  | Op -> 9
  | _ -> raise Unknown

let equal _ s1 s2 =
  match s1, s2 with
  | CstRat q1, CstRat q2 -> Q.equal q1 q2
  | _ -> (id s1) = (id s2)

let compare _ s1 s2 =
  match s1, s2 with
  | CstRat q1, CstRat q2 -> Q.compare q1 q2
  | _ -> compare (id s1) (id s2)

let arity = function
  | CstRat _                      -> Sorts.Rat, []
  | Ge | Le | Gt | Lt             -> Syntax.Sorts.Prop, [Sorts.Rat; Sorts.Rat]
  | Plus | Minus | Times | Divide -> Sorts.Rat, [Sorts.Rat; Sorts.Rat]
  | Op                            -> Sorts.Rat, [Sorts.Rat]
  | _ -> raise Unknown

let pp_latex fmt = function
  | CstRat i -> fprintf fmt "%a" Q.pp_print i
  | Plus     -> fprintf fmt "+"
  | Minus    -> fprintf fmt "-"
  | Times    -> fprintf fmt "\\times"
  | Divide   -> fprintf fmt "\\div"
  | Op       -> fprintf fmt "-"
  | Ge       -> fprintf fmt "\\geq"
  | Gt       -> fprintf fmt ">"
  | Le       -> fprintf fmt "\\leq"
  | Lt       -> fprintf fmt "<"
  | _ -> raise Unknown

let pp_utf8 fmt = function
  | CstRat i    -> fprintf fmt "%a" Q.pp_print i
  | Plus        -> fprintf fmt "+"
  | Minus       -> fprintf fmt "-"
  | Times       -> fprintf fmt "×"
  | Divide      -> fprintf fmt "÷"
  | Op          -> fprintf fmt "-"
  | Ge          -> fprintf fmt "≥"
  | Gt          -> fprintf fmt ">"
  | Le          -> fprintf fmt "≤"
  | Lt          -> fprintf fmt "<"
  | _ -> raise Unknown

let pp = pp_utf8
