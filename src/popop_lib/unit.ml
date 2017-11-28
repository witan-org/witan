(*************************************************************************)
(*  This file is part of Witan.                                          *)
(*                                                                       *)
(*  Copyright (C) 2017                                                   *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies           *)
(*           alternatives)                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en        *)
(*           Automatique)                                                *)
(*    CNRS  (Centre national de la recherche scientifique)               *)
(*                                                                       *)
(*  you can redistribute it and/or modify it under the terms of the GNU  *)
(*  Lesser General Public License as published by the Free Software      *)
(*  Foundation, version 2.1.                                             *)
(*                                                                       *)
(*  It is distributed in the hope that it will be useful,                *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(*  GNU Lesser General Public License for more details.                  *)
(*                                                                       *)
(*  See the GNU Lesser General Public License version 2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).           *)
(*************************************************************************)

type t = unit

let compare () () = 0
let equal () () = true
let hash _ = 0
let pp fmt () = Format.pp_print_string fmt "()"

module M = struct
  type key = unit
  type +'a t = 'a option
  type 'a data = 'a
  let empty = None
  let is_empty x = x = None
  let mem () t = t <> None
  let add () v _ = Some v
  let singleton () v = Some v
  let remove () _ = None
  let merge f m n = f () m n
  let compare cmp m n =
    match m, n with
    | None  , None -> 0
    | None  , _    -> 1
    | _     , None -> -1
    | Some m, Some n -> cmp m n
  let equal eq m n =
    match m, n with
    | None  , None -> true
    | Some m, Some n -> eq m n
    | _  , _    -> false
  let iter f = function None -> () | Some x -> f () x
  let fold f v acc = match v with None -> acc | Some x -> f () x acc
  let for_all f = function None -> true | Some v -> f () v
  let exists  f = function None -> false | Some v -> f () v
  let filter f = function Some v as o when f () v -> o | _ -> None
  let partition f = function Some v as o when f () v -> o,None | o -> None,o
  let cardinal = function None -> 0 | Some _ -> 1
  let bindings = function None -> [] | Some v -> [(),v]
  let min_binding = function None -> raise Not_found | Some v -> ((),v)
  let max_binding = min_binding
  let choose= min_binding
  let choose_rnd _ = min_binding
  let split () o = None,o,None
  let find () = function None -> raise Not_found | Some v -> v
  let map = Opt.map
  let mapi f = function None -> None | Some v -> Some (f () v)

    (** Added into why stdlib version *)
  let is_num_elt i v =
    match i,v with
    | 0, None | 1, Some _ -> true
    |_ -> false
  let find_remove () o = None,o
  let change f () o = f o
  let add_change empty add () b = function
    | None -> Some (empty b)
    | Some o -> Some (add b o)
  let union f m n = match m, n with
    | Some m, Some n -> f () m n
    | None, o | o, None -> o
  let inter f m n = match m, n with
    | Some m, Some n -> f () m n
    | _ -> None
  let diff f m n = match m, n with
    | Some m, Some n -> f () m n
    | _ -> m
  let submap f m n = match m, n with
    | Some m, Some n -> f () m n
    | None, _ -> true
    | Some _, None -> false
  let disjoint f m n = match m, n with
    | Some m, Some n -> f () m n
    | _, _ -> true
  let set_union m n = match m, n with
    | Some _ , _ -> m
    | _          -> n
  let set_inter m n = match m, n with
    | Some _ , Some _ -> m
    | _               -> None
  let set_diff m n = match m, n with
    | Some _ , None   -> m
    | _               -> None
  let set_submap m n = match m, n with
    | Some _ , None   -> false
    | _               -> true
  let set_disjoint m n = match m, n with
    | Some _ , Some _ -> false
    | _               -> true
  let set_equal m n = match m, n with
    | Some _ , Some _ | None, None -> true
    | _               -> false
  let set_compare m n = match m, n with
    | Some _ , Some _ | None, None-> 0
    | None, _ -> -1
    | _       -> 1
  let find_def v () = function
    | Some v -> v
    | None   -> v
  let find_opt () o = o
  let find_exn exn () = function
    | None -> raise exn
    | Some v -> v
  let map_filter f = function
    | Some v -> f v
    | None   -> None
  let mapi_filter f = function
    | Some v -> f () v
    | None   -> None
  let mapi_fold f m acc =
    match m with
    | Some v -> let acc,v = f () v acc in acc, Some v
    | None -> acc, None
  let fold2_inter _f = assert false
  let fold2_union _f = assert false
  let translate _ o = o
  let mapi_filter_fold _f = assert false
  let add_new exn () v = function
    | Some _ -> raise exn
    | None -> Some v
  let add_opt () o _ = o
  let keys = function | None -> [] | Some _ -> [()]
  let values = function | None -> [] | Some v -> [v]
  let union_merge _f = assert false
  let find_smaller_opt _ = function None -> None | Some d -> Some((),d)
  type 'a enumeration
  let val_enum _ = assert false
  let start_enum _ = assert false
  let next_enum _ = assert false
  let start_ge_enum _ = assert false
  let next_ge_enum _ = assert false
  let fold_left _ = assert false
  let fold_decr _ = assert false
  let of_list = function
    | [] -> None
    | ((),v)::_ -> Some v
  let check_invariant _ = true
  let pp pp fmt = function
    | None -> Format.pp_print_string fmt "{}"
    | Some v -> Format.fprintf fmt "{%a}" pp v
end


module S =
struct

  module M = M
  type elt = unit
  type set = unit M.t
  type t = set

  open M

  let is_true b = if b then Some () else None
  let is_some o = o <> None
  let const f e _ = f e

  let empty = empty
  let is_empty = is_empty
  let mem = mem
  let add e s = add e () s
  let singleton e = singleton e ()
  let remove = remove
  let merge f = merge (fun e a b ->
      is_true (f e (is_some a) (is_some b)))
  let compare = compare (fun _ _ -> 0)
  let equal = equal (fun _ _ -> true)
  let subset = submap (fun _ _ _ -> true)
  let disjoint = disjoint (fun _ _ _ -> false)
  let iter f = iter (const f)
  let fold f = fold (const f)
  let for_all f = for_all (const f)
  let exists f = exists (const f)
  let filter f = filter (const f)
  let partition f = partition (const f)
  let cardinal = cardinal
  let elements = keys
  let min_elt t = fst (min_binding t)
  let max_elt t = fst (max_binding t)
  let choose t = fst (choose t)
  let split e t = let l,m,r = split e t in l,(m <> None),r
  let change f x s = change (fun a -> is_true (f (is_some a))) x s
  let union = union (fun _ _ _ -> Some ())
  let inter = inter (fun _ _ _ -> Some ())
  let diff = diff (fun _ _ _ -> None)
  let translate = translate
  let add_new e x s = add_new e x () s
  let is_num_elt n m = is_num_elt n m
  let fold_left f = fold_left (fun accu k () -> f accu k)
  let fold2_inter f n m acc = if is_some n && is_some m then f () acc else acc
  let fold2_union f n m acc = if is_some n || is_some m then f () acc else acc
  let of_list l =
    List.fold_left (fun acc a -> add a acc) empty l
  let pp fmt = function
    | Some () -> Format.fprintf fmt "true"
    | None -> Format.fprintf fmt "false"
end


module H = struct
  type key = unit
  type 'a t = { mutable c : 'a list }
  (* because of caml hastbl semantic for add *)

  let is_empty s = s.c = []
  let copy s = { c = s.c }
  let reset s = s.c <- []
  let clear s = s.c <- []
  let create _ = { c = [] }
  let memo _ = assert false
  let mapi f s = s.c <- List.map (fun e -> f () e) s.c
  let stats _ = assert false
  let length _ = assert false
  let fold _ = assert false
  let iter f s = List.fold_left f () s.c
  let mem s () = not (is_empty s)
  let replace s () v = match s.c with [] -> s.c <- [v] | _::l -> s.c <- v::l
  let find_all _ = assert false
  let find s () = match s.c with [] -> raise Not_found | a::_ -> a
  let find_exn s exn () = match s.c with [] -> raise exn | a::_ -> a
  let find_opt _ = assert false
  let find_def _ = assert false
  let remove s () = match s.c with [] -> () | _::l -> s.c <- l
  let add s () v = s.c <- v::s.c
  let remove_all s () = clear s
  let change f s () = match s.c with
    | []   -> (match f None with None -> () | Some v -> s.c <- [v])
    | a::l -> (match f (Some a) with None -> s.c <- l | Some v -> s.c <- v::l)
  let add_new e s () v =
    match s.c with
    | [] -> add s () v
    | _ -> raise e

end
