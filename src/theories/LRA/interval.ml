(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2013                                                    *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Witan_stdlib.Std

type bound = Interval_sig.bound = Strict | Large

let pp_bound fmt = function
  | Strict -> Pp.string fmt "<"
  | Large -> Pp.string fmt "≤"

let compare_bounds_inf (e1,b1) (e2,b2) =
  let c = Q.compare e1 e2 in
  if c <> 0 then c
  else if b1 = Large && b2 = Strict then -1
  else if b1 = Strict && b2 = Large then 1
  else 0

let compare_bounds_sup (e1,b1) (e2,b2) =
  let c = Q.compare e1 e2 in
  if c <> 0 then c
  else if b1 = Large && b2 = Strict then 1
  else if b1 = Strict && b2 = Large then -1
  else 0

let compare_bounds_inf_sup (e1,b1) (e2,b2) =
  let c = Q.compare e1 e2 in
  if c <> 0 then c
  else
    match b1,b2 with
    | Large, Strict -> 1
    | Strict, Large -> 1
    | Large, Large -> 0
    | Strict, Strict -> 1


module Convexe = struct

  type t = { minb : bound; minv: Q.t; maxv: Q.t; maxb: bound }

  let pp fmt t =
    let print_bound_left fmt = function
      | Large  -> Format.fprintf fmt "["
      | Strict -> Format.fprintf fmt "]" in
    let print_bound_right fmt = function
      | Large  -> Format.fprintf fmt "]"
      | Strict -> Format.fprintf fmt "[" in
    if t.minb=Large && t.maxb=Large && Q.equal t.minv t.maxv
    then Format.fprintf fmt "{%a}" Q.pp t.minv
    else
    Format.fprintf fmt "%a%a;%a%a"
      print_bound_left t.minb
      Q.pp t.minv
      Q.pp t.maxv
      print_bound_right t.maxb

  let compare e1 e2 =
    let compare_bound b1 b2 = match b1, b2 with
      | Strict, Large -> 1
      | Large, Strict -> -1
      | _             -> 0
    in
    let c = Q.compare e1.minv e2.minv in
    if c = 0 then c else
    let c = compare_bound e1.minb e2.minb in
    if c = 0 then c else
    let c = Q.compare e1.maxv e2.maxv in
    if c = 0 then c else
    let c = compare_bound e1.maxb e2.maxb in
    c

  let equal e1 e2 =
    e1.minb == e2.minb && e1.maxb == e2.maxb &&
    Q.equal e1.minv e2.minv && Q.equal e1.maxv e2.maxv

  let hash e =
    let hash_bound = function
      | Large -> 1
      | Strict -> 3 in
    Hashcons.combine3
      (hash_bound e.minb) (hash_bound e.maxb)
      (Hashtbl.hash e.minv) (Hashtbl.hash e.maxv)


  include Stdlib.MkDatatype(struct
      type nonrec t = t
      let equal = equal let compare = compare
      let hash = hash let pp = pp
    end)

  let invariant e =
    not (Q.equal e.minv Q.inf) &&
    not (Q.equal e.maxv Q.minus_inf) &&
    let c = Q.compare e.minv e.maxv in
    if c = 0
    then e.minb = Large && e.maxb = Large
    else c < 0

  let is_singleton = function
    | {minv;maxv} when Q.equal minv maxv -> Some minv
    | _ -> None

  let singleton q =
    let t = {minb=Large; minv = q; maxv = q; maxb= Large} in
    assert (invariant t);
    t

  let except e x =
    let is_min = Q.equal e.minv x in
    let is_max = Q.equal e.maxv x in
    if is_min && is_max then None
    else if is_min
    then Some {e with minb=Strict}
    else if Q.equal e.maxv x
    then Some {e with maxb=Strict}
    else Some e

  let lower_min_max e1 e2 =
    let c = Q.compare e1.minv e2.minv in
    match e1.minb, e2.minb with
    | Strict, Large when c =  0 -> e2,e1
    | _             when c <= 0 -> e1,e2
    | _                         -> e2,e1

  let bigger_min_max e1 e2 =
    let c = Q.compare e1.maxv e2.maxv in
    match e1.maxb, e2.maxb with
    | Strict, Large when c =  0 -> e1,e2
    | _             when c >= 0 -> e2,e1
    | _                         -> e1,e2


  let is_distinct e1 e2 =
    (** x1 in e1, x2 in e2 *)
    (** order by the minimum *)
    let emin,emax = lower_min_max e1 e2 in (** emin.minv <= emax.minv *)
    (** look for inclusion *)
    let c = Q.compare emin.maxv emax.minv in
    match emin.maxb, emax.minb with
    (** emin.minv <? e1 <? emin.maxv < emax.minv <? e2 <? emax.maxv *)
    | _ when c <  0 -> true
    (** emin.minv <? e1 <  emin.maxv = emax.minv <  e2 <? emax.maxv *)
    (** emin.minv <? e1 <  emin.maxv = emax.minv <= e2 <? emax.maxv *)
    (** emin.minv <? e1 <= emin.maxv = emax.minv <  e2 <? emax.maxv *)
    | Strict, Strict | Strict, Large | Large, Strict
      when c = 0 -> true
    | _ -> false

  let is_included e1 e2 =
    assert (invariant e1);
    assert (invariant e2);
    compare_bounds_inf (e2.minv,e2.minb) (e1.minv,e1.minb) <= 0 &&
    compare_bounds_sup (e1.maxv,e1.maxb) (e2.maxv,e2.maxb) <= 0

  let mem x e =
    (match e.minb with
     | Strict -> Q.lt e.minv x
     | Large  -> Q.le e.minv x)
    &&
    (match e.maxb with
     | Strict -> Q.lt x e.maxv
     | Large  -> Q.le x e.maxv)

  let mult_pos q e =
    {e with minv = Q.mul e.minv q; maxv = Q.mul e.maxv q}

  let mult_neg q e =
    { minb = e.maxb; maxb = e.minb;
      minv = Q.mul e.maxv q;
      maxv = Q.mul e.minv q }

  let mult_cst q e =
    assert (Q.is_real q);
    let c = Q.sign q in
    if c = 0      then singleton Q.zero
    else if c > 0 then mult_pos q e
    else               mult_neg q e

  let add_cst q e =
    {e with minv = Q.add e.minv q; maxv = Q.add e.maxv q}

  let mult_bound b1 b2 =
    match b1, b2 with
    | Large , Large  -> Large
    | _              -> Strict

  let add e1 e2 =
    let t = {minb = mult_bound e1.minb e2.minb;
             minv = Q.add e1.minv e2.minv;
             maxv = Q.add e1.maxv e2.maxv;
             maxb = mult_bound e1.maxb e2.maxb} in
    assert (invariant t); t

  let minus e1 e2 =
    add e1 (mult_neg Q.minus_one e2)

  let gt q =
    let t = {minb=Strict; minv = q; maxv = Q.inf; maxb= Strict} in
    assert (invariant t); t

  let ge q =
    let t = {minb=Large; minv = q; maxv = Q.inf; maxb= Strict} in
    assert (invariant t); t

  let lt q =
    let t = {minb=Strict; minv = Q.minus_inf; maxv = q; maxb= Strict} in
    assert (invariant t); t

  let le q =
    let t = {minb=Strict; minv = Q.minus_inf; maxv = q; maxb= Large} in
    assert (invariant t); t

  let union e1 e2 =
    let emin,_ = lower_min_max  e1 e2 in
    let _,emax = bigger_min_max e1 e2 in
    {minb = emin.minb; minv = emin.minv;
     maxv = emax.maxv; maxb = emax.maxb}

  let inter e1 e2 =
    let (minv,minb) as min =
      if compare_bounds_inf (e1.minv,e1.minb) (e2.minv,e2.minb) < 0
      then (e2.minv,e2.minb) else (e1.minv,e1.minb)
    in
    let (maxv,maxb) as max =
      if compare_bounds_sup (e1.maxv,e1.maxb) (e2.maxv,e2.maxb) < 0
      then (e1.maxv,e1.maxb) else (e2.maxv,e2.maxb)
    in
    if compare_bounds_inf_sup min max > 0
    then None
    else if Q.equal minv maxv && minb = Large && maxb = Large
    then Some (singleton minv)
    else Some {minv;minb;maxv;maxb}

  let inter e1 e2 =
    let r = inter e1 e2 in
    assert (Opt.for_all invariant r);
    r

  (** intersection set.
      if the two arguments are equals, return the second
  *)

  let zero = singleton Q.zero
  let reals = {minb=Strict; minv=Q.minus_inf; maxb=Strict; maxv=Q.inf}
  let () = assert (invariant reals)

  let is_reals = function
    | {minb=Strict; minv; maxb=Strict; maxv}
      when Q.equal minv Q.minus_inf &&
           Q.equal maxv Q.inf     -> true
    | _ -> false

  let choose = function
    | {minb=Large;minv} -> minv
    | {maxb=Large;maxv} -> maxv
    | {minv;maxv} when Q.equal Q.minus_inf minv && Q.equal Q.inf maxv ->
      Q.zero
    | {minv;maxv} when Q.equal Q.minus_inf minv ->
      Q.make (Z.sub (Q.to_bigint maxv) Z.one) Z.one
    | {minv;maxv} when Q.equal Q.inf maxv ->
      Q.make (Z.add (Q.to_bigint minv) Z.one) Z.one
    | {minv;maxv} ->
      let q = Q.make (Z.add Z.one (Q.to_bigint minv)) Z.one in
      if Q.lt q maxv then q
      else Q.add maxv (Q.div_2exp (Q.sub minv maxv) 1)


  let nb_incr = 100
  let z_nb_incr = Z.of_int nb_incr
  let choose_rnd rnd = function
    | {minv;maxv} when Q.equal Q.minus_inf minv ->
      Q.make (Z.sub (Z.of_int (rnd nb_incr)) (Q.to_bigint maxv)) Z.one
    | {minv;maxv} when Q.equal Q.inf maxv ->
      Q.make (Z.add (Z.of_int (rnd nb_incr)) (Q.to_bigint minv)) Z.one
    | {minv;maxv} when Q.equal minv maxv -> minv
    | {minv;maxv} ->
      let d = Q.sub maxv minv in
      let i = 1 + rnd (nb_incr - 2) in
      let x = Q.make (Z.of_int i) (Z.of_int 100) in
      let q = Q.add minv (Q.mul x d) in
      assert (Q.lt minv q);
      assert (Q.lt q maxv);
      q

  let get_convexe_hull e =
    let mk v b =
      match Q.classify v with
      | Q.ZERO | Q.NZERO -> Some (v,b)
      | Q.INF | Q.MINF -> None
      | Q.UNDEF -> assert false in
    mk e.minv e.minb, mk e.maxv e.maxb

end

module ConvexeWithExceptions = struct

  type t = {con: Convexe.t; exc: Q.S.t}
  [@@deriving eq, ord]

  let pp fmt x =
    if Q.S.is_empty x.exc then Convexe.pp fmt x.con
    else
      Format.fprintf fmt "%a \ {%a}"
        Convexe.pp x.con
        (Pp.iter1 Q.S.iter Pp.comma Q.pp)
        x.exc

  let invariant x =
    Convexe.invariant x.con &&
    ( Q.S.is_empty x.exc ||
      ( Q.lt x.con.Convexe.minv (Q.S.min_elt x.exc) &&
        Q.lt (Q.S.max_elt x.exc) x.con.Convexe.maxv ))

  let hash e = 53 * (Convexe.hash e.con) +
               31 * (Q.S.fold_left (fun acc x -> 3*acc + Q.hash x) 5 e.exc)

  include Stdlib.MkDatatype(struct
      type nonrec t = t
      let equal = equal let compare = compare
      let hash = hash let pp = pp
    end)

  let of_con con = {con;exc=Q.S.empty}
  let reals = of_con Convexe.reals
  let from_convexe f x = of_con (f x)
  let singleton = from_convexe Convexe.singleton
  let zero = singleton Q.zero
  let le = from_convexe Convexe.le
  let lt = from_convexe Convexe.lt
  let ge = from_convexe Convexe.ge
  let gt = from_convexe Convexe.gt
  let is_singleton e = Convexe.is_singleton e.con
  let is_reals e = Convexe.is_reals e.con && Q.S.is_empty e.exc
  let mem x e = Convexe.mem x e.con && not (Q.S.mem x e.exc)

  let except e x =
    match Convexe.except e.con x with
    | None -> None
    | Some con ->
      if Convexe.mem x con
      then Some {con; exc = Q.S.add x e.exc}
      else Some {e with con}

  let union e1 e2 =
    {con=Convexe.union e1.con e2.con;
     exc=Q.S.inter e1.exc e2.exc}

  let normalize {con;exc} =
    match Convexe.is_singleton con with
    | Some s -> if Q.S.mem s exc then None else Some (of_con con)
    | None ->
      let _,has_min,exc = Q.S.split con.Convexe.minv exc in
      let _,has_max,exc = Q.S.split con.Convexe.maxv exc in
      Some {exc; con = {con with minb = if has_min then Strict else con.minb;
                                 maxb = if has_max then Strict else con.maxb }}
  let inter e1 e2 =
    match Convexe.inter e1.con e2.con with
    | None -> None
    | Some con ->
    normalize { con; exc = Q.S.union e1.exc e2.exc }

  let is_distinct e1 e2 =
    Convexe.is_distinct e1.con e2.con ||
    not (Q.S.equal e1.exc e2.exc)

  let is_included e1 e2 =
    Convexe.is_included e1.con e2.con ||
    Q.S.subset e1.exc e2.exc

  let add e1 e2 =
    match Convexe.is_singleton e1.con, e1, Convexe.is_singleton e2.con, e2 with
    | Some s1, _, Some s2, _ -> singleton (Q.add s1 s2)
    | None, e, Some s, _ | Some s, _, None, e ->
      {con=Convexe.add_cst s e.con; exc = Q.S.translate (Q.add s) e.exc}
    | _ -> of_con (Convexe.add e1.con e2.con)

  let add_cst s e =
    {con = Convexe.add_cst s e.con; exc = Q.S.translate (Q.add s) e.exc}

  let minus e1 e2 =
    match Convexe.is_singleton e1.con, e1, Convexe.is_singleton e2.con, e2 with
    | Some s1, _, Some s2, _ ->
      singleton (Q.sub s1 s2)
    | None, e, Some s, _ ->
        { con = Convexe.add_cst (Q.neg s) e.con;
          exc = Q.S.translate (fun x -> Q.sub x s) e.exc }
    | Some s, _, None, e ->
      { con = Convexe.add_cst s e.con;
        exc =
          Q.S.fold_left (fun acc exc -> Q.S.add (Q.sub s exc) acc)
            Q.S.empty e.exc }
    | _ -> of_con (Convexe.add e1.con e2.con)

  let mult_cst s e =
    {con = Convexe.mult_cst s e.con;
     exc =
       Q.S.fold_left (fun acc exc -> Q.S.add (Q.mul s exc) acc)
         Q.S.empty e.exc }

  let choose e =
    let con = if Q.S.is_empty e.exc then e.con
      else (** by the invariant the intersection must succeed *)
        Opt.get_exn Impossible
          (Convexe.inter e.con (Convexe.lt (Q.S.min_elt e.exc))) in
    Convexe.choose con

  let choose_rnd rnd e =
    let con = if Q.S.is_empty e.exc then e.con
      else (** by the invariant the intersection must succeed *)
        Opt.get_exn Impossible
          (Convexe.inter e.con (Convexe.lt (Q.S.min_elt e.exc))) in
    Convexe.choose_rnd rnd con

  let get_convexe_hull e = Convexe.get_convexe_hull e.con

end

module Union = struct

  type t = Convexe.t list [@@ deriving ord]

  (** all t returned to the outside should verify this invariant *)
  let invariant = function
    | [] -> false
    | l ->
      let rec aux minb' minv' (l:t) =
        match minb', l with
        | Large, [] when Q.equal minv' Q.inf -> false
        | _    , []                                  -> true
        | _     , {minb=Large ; minv}::_ when Q.compare minv minv' <= 0 -> false
        | Large , {minb=Strict; minv}::_ when Q.compare minv minv' <= 0 -> false
        | Strict, {minb=Strict; minv}::_ when Q.compare minv minv' <  0 -> false
        | _, ({maxv;maxb} as e)::l ->
          Convexe.invariant e && aux maxb maxv l
      in
      aux Strict Q.minus_inf l

  let pp fmt l =
    Pp.list (Pp.constant_string "∪") Convexe.pp fmt l

  let equal l1 l2 =
    List.length l1 = List.length l2 &&
    List.for_all2 Convexe.equal l1 l2

  let is_singleton = function
    | [r] -> Convexe.is_singleton r
    | _ -> None

  let is_distinct t1 t2 =
    List.length t1 <> List.length t2 ||
    List.exists2 Convexe.is_distinct t1 t2

  let is_included _t1 _t2 =
    raise (TODO "Interval.Union.is_included")

  let mem x e = List.exists (fun e -> Convexe.mem x e) e

  let lower_min_max e1 l1 t1 e2 l2 t2 =
    let c = Q.compare e1.Convexe.minv e2.Convexe.minv in
    match e1.Convexe.minb, e2.Convexe.minb with
    | Strict, Large
      when c =  0 -> e2,l2,t2,e1,l1,t1
    | _
      when c <= 0 -> e1,l1,t1,e2,l2,t2
    | _           -> e2,l2,t2,e1,l1,t1

  let rec union t1 t2 =
    match t1,t2 with
    | [], l | l, [] -> l
    | e1::l1, e2::l2 ->
      (** order by the minimum *)
      let emin,lmin,_,emax,lmax,tmax =
        lower_min_max e1 l1 t1 e2 l2 t2 in
      (** look for an intersection *)
      let c = Q.compare emin.maxv emax.minv in
      match emin.maxb, emax.minb with
      (** no intersection *)
      | Strict, Strict
        when c <= 0 -> emin::(union lmin tmax)
      | Large,Large | Strict, Large | Large, Strict
        when c <  0 -> emin::(union lmin tmax)
      | _ ->
        (** intersection *)
        (** look for inclusion *)
        let c = Q.compare emax.maxv emin.maxv in
        match emax.maxb, emin.maxb with
        (** max included in min *)
        | Strict, Strict | Large, Large | Strict, Large
          when c <= 0 -> emin::(union lmin lmax)
        | Large, Strict
          when c < 0 -> emin::(union lmin lmax)
        (** merge the two *)
        | _ ->
          let e = {Convexe.minv = emin.minv; minb = emin.minb;
                   maxv = emax.maxv; maxb = emax.maxb} in
          union lmin (e::lmax)

  let union t1 t2 =
    let r = union t1 t2 in
    assert (invariant r);
    r

  let rec inter' t1 t2 =
    match t1,t2 with
    | [], _ | _, [] -> []
    | e1::l1, e2::l2 ->
      (** order by the minimum *)
      let emin,lmin,tmin,emax,lmax,tmax = lower_min_max e1 l1 t1 e2 l2 t2 in
      (** look for an intersection *)
      let c = Q.compare emin.maxv emax.minv in
      match emin.maxb, emax.minb with
      (** no intersection *)
      | Strict, Strict | Strict, Large | Large, Strict
        when c <= 0 -> inter' lmin tmax
      | Large,Large
        when c <  0 -> inter' lmin tmax
      | _ ->
        (** intersection *)
        (** look for inclusion *)
        let c = Q.compare emax.maxv emin.maxv in
        match emax.maxb, emin.maxb with
        (** max included in min *)
        | Strict, Strict | Large, Large | Strict, Large
          when c <= 0 -> emax::(inter' tmin lmax)
        | Large, Strict
          when c < 0 -> emax::(inter' tmin lmax)
        (** overlapping strictly *)
        | _ ->
          let e = {Convexe.minv = emax.minv; minb = emax.minb;
                   maxv = emin.maxv; maxb = emin.maxb } in
          e::(inter' lmin tmax)

  (* (\** special case if the two are equals, return the second *\) *)
  (* let rec inter t1 t2 = *)
  (*   match t1, t2 with *)
  (*   | _ when t1 == t2 -> t2 *)
  (*   | [], _ | _, [] -> [] *)
  (*   | e1::l1, e2::l2 when equal_inter e1 e2 -> *)
  (*     let l = inter l1 l2 in *)
  (*     if l == l2 then t2 else e2::l2 *)
  (*   | _ -> inter' t1 t2 *)

  let inter t1 t2 =
    let r = inter' t1 t2 in
    match r with
    | [] -> None
    | _ ->
      assert (invariant r);
      Some r

  let except e x =
    inter e [Convexe.lt x; Convexe.gt x]

  let singleton q =
    let t = [Convexe.singleton q] in
    assert (invariant t);
    t

  let zero = singleton Q.zero

  let reals = [Convexe.reals]
  let is_reals = function
    | [r] when Convexe.is_reals r -> true
    | _ -> false

  let gt q =
    let t = [Convexe.gt q] in
    assert (invariant t);
    t

  let ge q =
    let t = [Convexe.ge q] in
    assert (invariant t);
    t

  let lt q =
    let t = [Convexe.lt q] in
    assert (invariant t);
    t

  let le q =
    let t = [Convexe.le q] in
    assert (invariant t);
    t

  let add_cst q = List.map (Convexe.add_cst q)

  let add_cst q t =
    let r = add_cst q t in
    assert (invariant r);
    r

  let mult_pos q = List.map (Convexe.mult_pos q)

  let mult_neg q = List.rev_map (Convexe.mult_neg q)

  let mult_cst q t =
    assert (Q.is_real q);
    let c = Q.sign q in
    if c = 0 then singleton Q.zero
    else if c > 0 then mult_pos q t
    else               mult_neg q t

  let mult_cst q t =
    let r = mult_cst q t in
    assert (invariant r);
    r

  (** t is smaller than l but perhaps a merge is needed *)
  let cons (t:Convexe.t) (l:t) =
    match t.maxb, l with
    | _,[] -> [t]
    | Strict, ({minb=Strict} as e)::_ when Q.compare t.maxv e.minv <= 0 ->
      t::l
    | _, e::_                       when Q.compare t.maxv e.minv <  0 ->
      t::l
    | _, e::l ->
      assert (Q.compare t.minv e.minv < 0);
      assert (Q.compare t.maxv e.maxv < 0);
      {minb=t.minb; minv = t.maxv; maxv = e.maxv; maxb = e.maxb}::l

  let rec add_intemaxval t = function
    | [] -> []
    | e::l ->
      let e = Convexe.add t e in
      cons e (add_intemaxval t l)

  let add t1 t2 =
    let res = match is_singleton t1, t1, is_singleton t2, t2 with
      | None,_, None,_ ->
        List.fold_left (fun acc t -> union acc (add_intemaxval t t2)) [] t1
      | Some q, _, None, t
      | None, t, Some q, _ when Q.equal Q.zero q -> t
      | Some q, _, None, t
      | None, t, Some q, _ ->
        add_cst q t
      | Some q1,_, Some q2,_ -> singleton (Q.add q1 q2)
    in
    assert ( invariant res );
    res

  let minus t1 t2 =
    add t1 (mult_neg Q.minus_one t2)

  (** TODO better heuristic *)
  let choose = function
    | [] -> assert false
    | {Convexe.minb=Large;minv}::_ -> minv
    | {maxb=Large;maxv}::_ -> maxv
    | {minv;maxv}::_ when Q.equal Q.minus_inf minv ->
      Q.make (Z.sub Z.one (Q.to_bigint maxv)) Z.one
    | {minv;maxv}::_ when Q.equal Q.inf maxv ->
      Q.make (Z.add Z.one (Q.to_bigint minv)) Z.one
    | {minv;maxv}::_ -> Q.add maxv (Q.div_2exp (Q.sub minv maxv) 1)

  let choose_rnd rnd l =
    Convexe.choose_rnd rnd (List.nth l (rnd (List.length l)))

  let get_convexe_hull e =
    match e with
    | [] -> assert false
    | e1::l ->
      let s,_ = Convexe.get_convexe_hull e1 in
      let last = List.fold_left (fun _ e -> e) e1 l in
      let _,l = Convexe.get_convexe_hull last in
      s,l

  let hash = List.fold_left (fun acc e -> 3*acc + 7*Convexe.hash e) 65553
  include Stdlib.MkDatatype(struct
      type nonrec t = t
      let equal = equal
      let compare = compare
      let hash = hash
      let pp = pp
    end)
end
