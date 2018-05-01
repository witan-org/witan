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

(* Proof terms *)
(* ************************************************************************ *)

open Witan_popop_lib
    
type binder =
  | Pi      (** Dependant product *)
  | Arrow   (** Non-dependant product *)
  | Lambda  (** Function abstraction *)
  | Forall  (** Universal quantification *)
  | Exists  (** Existencial quantification *)

and id = t Id.id

and descr =
  | Type
  | Id of id
  | App of t * t
  | Let of id * t * t
  | Binder of binder * id * t

and t = {
  ty : t;
  term : descr;
  mutable hash : int;
}
[@@deriving eq]

exception Function_expected of t
exception Type_mismatch of t * t


(* Std functions *)
(* ************************************************************************ *)

(** Wrapper around the id module to be given to functor such as Map/Set
    which expect a non-polymorphic type. *)
module Tmp = struct
  type t = id
  let hash = Id.hash
  let equal = Id.equal
  let compare = Id.compare
  let pp = Id.pp
end

module Id = struct
  include Tmp
  include Stdlib.MkDatatype(Tmp)
  include (Id : (module type of Id) with type 'a t := 'a Id.t)
end

(** Memoised hash function *)
let rec hash_aux t =
  match t.term with
  | Type -> 0
  | Id id -> Id.hash id
  | App (f, arg) ->
    CCHash.pair hash hash (f, arg)
  | Let (x, v, body) ->
    CCHash.triple Id.hash hash hash (x, v, body)
  | Binder (b, var, body) ->
    CCHash.triple CCHash.poly Id.hash hash (b, var, body)

and hash t =
  if t.hash > 0 then t.hash
  else begin
    let h = hash_aux t in
    t.hash <- h;
    h
  end

let _descr = function
  | Type -> 0
  | Id _ -> 1
  | App _ -> 2
  | Let _ -> 3
  | Binder _ -> 4

let rec compare_aux t t' =
  match t.term, t'.term with
  | Type, Type -> 0
  | Id x, Id y -> Id.compare x y
  | App (f, arg), App (f', arg') ->
    CCOrd.Infix.(compare f f'
                 <?> (compare, arg, arg'))
  | Let (x, v, body), Let (x', v', body') ->
    CCOrd.Infix.(Id.compare x x'
                 <?> (compare, v, v')
                 <?> (compare, body, body'))
  | Binder (b, v, body), Binder (b', v', body') ->
    CCOrd.Infix.(Pervasives.compare b b'
                 <?> (Id.compare, v, v')
                 <?> (compare, body, body'))
  | u, v -> Pervasives.compare (_descr u) (_descr v)

and compare t t' =
  if CCEqual.physical t t' then 0
  else
    CCOrd.(Pervasives.compare (hash t) (hash t')
           <?> (compare_aux, t, t'))

let equal t t' = compare t t' = 0

(* Bound variables *)
(* ************************************************************************ *)

let rec free_vars acc t =
  match t.term with
  | Type -> acc
  | Id v -> Id.S.add v acc
  | App (f, arg) ->
    free_vars (free_vars acc f) arg
  | Let (v, e, body) ->
    Id.S.remove v (free_vars (free_vars acc e) body)
  | Binder (_, v, body) ->
    Id.S.remove v (free_vars acc body)


(** Creating terms *)
let rec _Type = {
  ty = _Type;
  term = Type;
  hash = -1;
}

let mk ty term =
  { ty; term; hash = -1; }

let const v = mk (Id.ty v) (Id v)

let _Prop_id = Id.mk "Prop" _Type
let _Prop = const _Prop_id

let letin v e body =
  if equal (Id.ty v) e.ty then
    mk body.ty (Let (v, e, body))
  else
    raise (Type_mismatch (e, (Id.ty v)))

let rec bind b v body =
  match b with
  | Lambda ->
    let fv = free_vars Id.S.empty body.ty in
    let ty_b = if Id.S.mem v fv then Pi else Arrow in
    let res_ty = bind ty_b v body.ty in
    mk res_ty (Binder (b, v, body))
  | Pi | Arrow ->
    mk _Type (Binder (b, v, body))
  | Forall | Exists ->
    if equal _Prop body.ty then
      mk _Prop (Binder (b, v, body))
    else
      raise (Type_mismatch (body, _Prop))


(* Typing and application *)
(* ************************************************************************ *)

module Subst = Map.Make(Tmp)

let extract_fun_ty t =
  match t with
  | { term = Binder (Pi, v, ty); _ }
  | { term = Binder (Arrow, v, ty); _ } -> v, ty
  | _ -> raise (Function_expected t)

let rec app t arg =
  let v, ty = extract_fun_ty t.ty in
  let expected_arg_ty = Id.ty v in
  let actual_arg_ty = arg.ty in
  if equal expected_arg_ty actual_arg_ty then
    let s = Subst.singleton v arg in
    let res_ty = subst s ty in
    mk res_ty (App (t, arg))
  else
    raise (Type_mismatch (arg, expected_arg_ty))

and subst s t =
  match t.term with
  | Type -> t
  | Id v ->
    begin try Subst.find v s
      with Not_found -> t end
  | App (f, arg) ->
    app (subst s f) (subst s arg)
  | Let (v, e, body) ->
    let e' = subst s e in
    let s' = Subst.remove v s in
    let v', s'' =
      if equal e.ty e'.ty then v, s'
      else
        let v' = Id.mk (Id.name v) e'.ty in
        v', Subst.add v (const v') s'
    in
    let body' = subst s'' body in
    if CCEqual.physical v v' && CCEqual.physical e e' && CCEqual.physical body body' then t
    else letin v' e' body'
  | Binder (b, v, body) ->
    let ty = Id.ty v in
    let ty' = subst s ty in
    let s' = Subst.remove v s in
    let v', s'' =
      if equal ty ty' then v, s'
      else
        let v' = Id.mk (Id.name v) ty' in
        v', Subst.add v (const v') s'
    in
    let body' = subst s'' body in
    if CCEqual.physical v v' && CCEqual.physical body body' then t
    else bind b v' body'


(* Shorthands for constructors *)
(* ************************************************************************ *)

let apply f l = List.fold_left app f l

let rec apply_left f = function
  | [] -> assert false
  | [x] -> x
  | x :: y :: r ->
    apply_left f (apply f [x; y] :: r)

let pi v body = bind Pi v body
let pis l body = List.fold_right pi l body

let lambda v body = bind Lambda v body
let lambdas l body = List.fold_right lambda l body

let arrow ty ret = bind Arrow (Id.mk "_" ty) ret
let arrows l ret = List.fold_right arrow l ret

let forall v body = bind Forall v body
let foralls l body = List.fold_right forall l body

let exist v body = bind Exists v body
let exists l body = List.fold_right exist l body

(* Printing helpers *)
(* ************************************************************************ *)

let rec split_last = function
  | [] -> [], None
  | [x] -> [], Some x
  | x :: r ->
    let l, res = split_last r in
    x :: l, res


(* Term Uncurryfication *)
(* ************************************************************************ *)

(* TODO: Expose a 'view' function for
   first-order uncurried concatenated terms ? *)

let uncurry_app t =
  let rec aux acc = function
    | { term = App (f, arg); _ } ->
      aux (arg :: acc) f
    | t -> t, acc
  in
  aux [] t

let rec uncurry_assoc_left f = function
  | [] -> assert false
  | (x :: _) as l ->
    begin match uncurry_app x with
      | { term = Id f'; _ }, l' when Id.equal f f' ->
        uncurry_assoc_left f (l' @ l)
      | _ -> l
    end

let rec uncurry_assoc_right f l =
  match split_last l with
  | _, None -> assert false
  | r, Some x ->
    begin match uncurry_app x with
      | { term = Id f'; _ }, l' when Id.equal f f' ->
        r @ (uncurry_assoc_right f l')
      | _ -> l
    end

let _assoc _ = None

let uncurry ?(assoc=_assoc) t =
  match uncurry_app t with
  | ({ term = Id f; _ } as f_t), l ->
    let l = CCOpt.(
        map_or ~default:l (function
            | `Left -> uncurry_assoc_left f l
            | `Right -> uncurry_assoc_right f l)
          (assoc f)
      ) in
    (f_t, l)
  | (f_t, l) -> (f_t, l)

(* Binder concatenation *)
(* ************************************************************************ *)

let flatten_binder b t =
  let rec aux b acc = function
    | { term = Binder (b', v, body); _ } when (equal_binder b b') ->
      aux b (v :: acc) body
    | t -> List.rev acc, t
  in
  aux b [] t

let concat_aux ty l acc =
  match l with
  | [] -> acc
  | _ -> (ty, List.rev l) :: acc

let concat_vars l =
  let rec aux ty acc curr = function
    | [] ->
      List.rev (concat_aux ty curr acc)
    | v :: r ->
      if equal ty (Id.ty v)
      then aux ty acc (v :: curr) r
      else aux (Id.ty v) (concat_aux ty curr acc) [v] r
  in
  aux _Type [] [] l

(* Binder printing helpers *)
(* ************************************************************************ *)

let binder_name = function
  | Pi -> "Π"
  | Arrow -> "->"
  | Lambda -> "λ"
  | Forall -> "∀"
  | Exists -> "∃"

let binder_sep = function _ -> ","

(* Printing *)
(* ************************************************************************ *)

let print_type fmt () =
  Format.fprintf fmt "Type"

let print_id = Id.print

let rec print_app fmt t =
  let f, l = uncurry_app t in
  assert (nnil l);
  Format.fprintf fmt "(%a@ %a)"
    print f CCFormat.(list ~sep:(return "@ ") print) l

and print_let fmt v e body =
  Format.fprintf fmt "@[<v>@[<hov>let %a =@ %a in@]@ %a@]"
    print_id v print e print body

and print_arrow fmt t =
  let l, body = flatten_binder Arrow t in
  let l' = List.map Id.ty l in
  let sep fmt () = Format.fprintf fmt " ->@ " in
  Format.fprintf fmt "(@[<hov>%a ->@ %a@])"
    CCFormat.(list ~sep print) l' print body

and print_var_list fmt (ty, l) =
  assert (nnil l);
  Format.fprintf fmt "(%a :@ %a)"
    CCFormat.(list ~sep:(return "@ ") print_id) l print ty

and print_var_lists fmt l =
  CCFormat.(list ~sep:(return "@ ") print_var_list) fmt l

and print_binder fmt b t =
  let l, body = flatten_binder b t in
  let l' = concat_vars l in
  Format.fprintf fmt "(@[<hov 2>%s@[<hov>%a@]%s@ %a@])"
    (binder_name b) print_var_lists l'
    (binder_sep b) print body

and print fmt t =
  match t.term with
  | Type -> print_type fmt ()
  | Id v -> print_id fmt v
  | App _ -> print_app fmt t
  | Let (v, e, body) -> print_let fmt v e body
  | Binder (Arrow, _, _) -> print_arrow fmt t
  | Binder (b, _, _) -> print_binder fmt b t

let pp = print

(* Proof term constants *)
(* ************************************************************************ *)

let defined_fun = Id.H.create 16
let add_defined id = Id.H.add_new Std.Impossible defined_fun id ()
let is_defined id = Id.H.mem defined_fun id

let equal_id =
  let a_id = Id.mk "a" _Type in
  let a_type = const a_id in
  Id.mk "==" (pi a_id (arrows [a_type; a_type] _Prop))

let () = add_defined equal_id

let distinct_id_of_int = Stdlib.DInt.H.create 16
let int_of_distinct_id = Id.H.create 16

let distinct_id =
  Stdlib.DInt.H.memo (fun i ->
      let a_id = Id.mk "a" _Type in
      let a_type = const a_id in
      let args = CCList.replicate i (a_type) in
      let id = Id.mk "distinct" (pi a_id (arrows args _Prop)) in
      add_defined id;
      Id.H.add_new Std.Impossible int_of_distinct_id id i;
      id
    )
    distinct_id_of_int

let is_distinct_id id = Id.H.mem int_of_distinct_id id

let true_id = Id.mk "true" _Prop
let false_id = Id.mk "false" _Prop
let () = add_defined true_id; add_defined false_id

let not_id = Id.mk "not" (arrow _Prop _Prop)

let imply_id =
  Id.mk "->" (arrows [_Prop; _Prop] _Prop)

let equiv_id =
  Id.mk "<->" (arrows [_Prop; _Prop] _Prop)

let () = add_defined not_id; add_defined imply_id; add_defined equiv_id

let or_id_of_int = Stdlib.DInt.H.create 16
let int_of_or_id = Id.H.create 16

let or_id =
  Stdlib.DInt.H.memo (fun i ->
      let args = CCList.replicate i _Prop in
      let id = Id.mk "||" (arrows args _Prop) in
      add_defined id;
      Id.H.add_new Std.Impossible int_of_or_id id i;
      id
    )
    or_id_of_int

let is_or_id id = Id.H.mem int_of_or_id id

let mk_nary s arg res =
  let f_id_of_int = Stdlib.DInt.H.create 16 in
  let int_of_f_id = Id.H.create 16 in

  let f_id =
    Stdlib.DInt.H.memo (fun i ->
        let args = CCList.replicate i arg in
        let id = Id.mk s (arrows args res) in
        add_defined id;
        Id.H.add_new Std.Impossible int_of_f_id id i;
        id
      )
      f_id_of_int
  in

  let is_f_id id = Id.H.mem int_of_f_id id in

  let f_term i = const (f_id i) in
  let is_f_term = function
    | {term = Id id} -> is_f_id id
    | _ -> false in
  f_id, is_f_id, f_term, is_f_term


let and_id, is_and_id, and_term, is_and_term = mk_nary "&&" _Prop _Prop

let _Real_id = Id.mk "Real" _Type
let _Real = const _Real_id

let const_real_id_of_int = Stdlib.DStr.H.create 16
let int_of_const_real_id = Id.H.create 16

let const_real_id =
  (** TODO check the syntax *)
  Stdlib.DStr.H.memo (fun s ->
      let q = match Std.Q.of_string_decimal s with
        | None -> invalid_arg (Printf.sprintf "bad real:%s" s)
        | Some q -> q
      in
      let id = Id.mk s _Real in
      add_defined id;
      Id.H.add_new Std.Impossible int_of_const_real_id id q;
      id
    )
    const_real_id_of_int

let is_const_real_id id = Id.H.mem int_of_const_real_id id
let get_const_real_id id = Id.H.find int_of_const_real_id id

let add_real_id, is_add_real_id, add_real_term, is_add_real_term =
  mk_nary "add" _Real _Real
let lt_real_id, is_lt_real_id, lt_real_term, is_lt_real_term =
  mk_nary "<" _Real _Prop
let le_real_id, is_le_real_id, le_real_term, is_le_real_term =
  mk_nary "<=" _Real _Prop

let sub_real_id =
  Id.mk "sub" (arrows [_Real; _Real] _Real)
let neg_real_id =
  Id.mk "neg" (arrows [_Real] _Real)
let mul_real_id =
  Id.mk "mul" (arrows [_Real; _Real] _Real)
let div_real_id =
  Id.mk "div" (arrows [_Real; _Real] _Real)

let sub_real_term = const sub_real_id
let neg_real_term = const neg_real_id
let mul_real_term = const mul_real_id
let div_real_term = const div_real_id

let ite_id =
  let a_id = Id.mk "a" _Type in
  let a_type = const a_id in
  Id.mk "ite" (pi a_id (arrows [_Prop; a_type; a_type] a_type))

let () = add_defined ite_id

let or_term i = const (or_id i)
let is_or_term = function
  | {term = Id id} -> is_or_id id
  | _ -> false
let const_real_term i = const (const_real_id i)
let is_const_real_term = function
  | {term = Id id} -> is_const_real_id id
  | _ -> false
let get_const_real_term = function
  | {term = Id id} -> get_const_real_id id
  | _ -> raise Not_found
let not_term = const not_id
let true_term = const true_id
let false_term = const false_id
let equal_term = const equal_id
let distinct_term i = const (distinct_id i)
let is_distinct_term = function
  | {term = Id id} -> is_distinct_id id
  | _ -> false
let imply_term = const imply_id
let equiv_term = const equiv_id
let ite_term = const ite_id

include Stdlib.MkDatatype(struct
    type nonrec t = t
    let equal = equal
    let compare = compare
    let hash = hash
    let pp = pp
  end)

(* Module alias *)
(* ************************************************************************ *)

let () = Exn_printer.register (fun fmt exn ->
    match exn with
    | Type_mismatch (t,ty) ->
      Format.fprintf fmt "Type mismatch %a is not of type %a."
        print t print ty
    | exn -> raise exn
  )
