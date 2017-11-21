exception Unknown

let equal _ _ _ = raise Unknown

let compare _ _ _ = raise Unknown

let pp _ _ = function
  | _ -> raise Unknown
