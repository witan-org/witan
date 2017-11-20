(****************************)
(* Extensible type of sorts *)
(****************************)

open Format

type t = ..

type t +=
  | Prop
  | User of String.t
(*   | Fun  of t*(t list) 
 *   | BV of int *)

exception Unknown

let equal s1 s2 =
  match s1, s2 with
  | Prop, Prop -> true
  | User st1, User st2 -> String.equal st1 st2
  | Prop, User _ | User _, Prop -> false
  | _ -> raise Unknown

let compare s1 s2 =
  match s1, s2 with
  | Prop, Prop -> 0
  | User st1, User st2 -> String.compare st1 st2
  | Prop, User _ -> 1
  | User _, Prop -> -1
  | _ -> raise Unknown

let pp fmt = function
  | Prop -> fprintf fmt "{\\sf prop}"
  | User s -> fprintf fmt "\"%s\"" s
  | _ -> raise Unknown
(*   | Fun(o,i) ->
 *      fprintf fmt "(%a\rightarrow %a)"
 *        pp o
 *        pp_list i
 *   | BV i -> fprintf fmt "\"BV_{%i}\"" i
 * and pp_list fmt = function
 *   | []   -> fprintf fmt "()"
 *   | [so] -> fprintf fmt "%a" pp so
 *   | so::l-> fprintf fmt "%a,%a" pp so pp_list l *)

let show = show_of_pp pp
