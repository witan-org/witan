open Format

type Witan_core.Sorts.t += Rat

exception Unknown

let equal _ s1 s2 =
  match s1, s2 with
  | Rat, Rat -> true
  | _ -> raise Unknown

let compare _ s1 s2 =
  match s1, s2 with
  | Rat, Rat -> 0
  | _ -> raise Unknown

let pp _ fmt = function
  | Rat  -> fprintf fmt "{\\mathbb Q}"
  | _ -> raise Unknown
