(*************************************************************************)
(*                                                                       *)
(*  This file is part of Frama-C.                                        *)
(*                                                                       *)
(*  Copyright (C) 2007-2017                                              *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies             *)
(*         alternatives)                                                 *)
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
(*                                                                       *)
(*************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Bit library                                                        --- *)
(* -------------------------------------------------------------------------- *)

let hsb =
  let hsb p = if p land 2 != 0 then 1 else 0
  in let hsb p = let n = p lsr  2 in if n != 0 then 2 + hsb n else hsb p
  in let hsb p = let n = p lsr  4 in if n != 0 then 4 + hsb n else hsb p
  in let hsb = Array.init 256 hsb
  in let hsb p = let n = p lsr  8 in if n != 0 then  8 + hsb.(n) else hsb.(p)
  in let hsb p = let n = p lsr 16 in if n != 0 then 16 + hsb n else hsb p
  in match Sys.word_size with
  | 32 -> hsb
  | 64 -> (function p -> let n = p lsr 32 in
      if n != 0 then 32 + hsb n else hsb p)
  | _ -> assert false (** absurd: No other possible achitecture supported *)

let highest_bit x = 1 lsl (hsb x)
let lowest_bit x = x land (-x)

(* -------------------------------------------------------------------------- *)
(* --- Bit utilities                                                      --- *)
(* -------------------------------------------------------------------------- *)
let decode_mask p = lowest_bit (lnot p)

let branching_bit p0 p1 = highest_bit (p0 lxor p1)
let mask p m = (p lor (m-1)) land (lnot m)

let zero_bit_int k m = (k land m) == 0
let zero_bit k p = zero_bit_int k (decode_mask p)

let match_prefix_int k p m = (mask k m) == p
let match_prefix k p = match_prefix_int k p (decode_mask p)

let included_mask_int m n =
  (* m mask is strictly included into n *)
  (* can not use (m < n) when n is (1 lsl 62) = min_int < 0 *)
  (* must use (0 < (n-m) instead *)
  0 > n - m
(* let included_mask p q = included_mask_int (decode_mask p) (decode_mask q) *)

let included_prefix p q =
  let m = decode_mask p in
  let n = decode_mask q in
  included_mask_int m n && match_prefix_int q p m


(* -------------------------------------------------------------------------- *)
(* --- Debug                                                              --- *)
(* -------------------------------------------------------------------------- *)

let pp_mask m fmt p =
  begin
    let bits = Array.make 63 false in
    let last = ref 0 in
    for i = 0 to 62 do
      let u = 1 lsl i in
      if u land p <> 0 then
        bits.(i) <- true ;
      if u == m then last := i ;
    done ;
    Format.pp_print_char fmt '*' ;
    for i = !last - 1 downto 0 do
      Format.pp_print_char fmt (if bits.(i) then '1' else '0') ;
    done ;
  end

let pp_bits fmt k =
  begin
    let bits = Array.make 63 false in
    let last = ref 0 in
    for i = 0 to 62 do
      if (1 lsl i) land k <> 0 then
        ( bits.(i) <- true ;
          if i > !last then last := i ) ;
    done ;
    for i = !last downto 0 do
      Format.pp_print_char fmt (if bits.(i) then '1' else '0') ;
    done ;
  end

(* ---------------------------------------------------------------------- *)
(* --- Patricia Trees By L. Correnson & P. Baudin & F. Bobot          --- *)
(* ---------------------------------------------------------------------- *)

module Make(K:Map_intf.TaggedEqualType) :
  Map_intf.Gen_Map_hashcons with type NT.key = K.t = struct

  module Gen(G:sig
      type (+'a) t
      type 'a data
      type 'a view = private
        | Empty
        | Lf of K.t * 'a
        | Br of int * 'a t * 'a t
      val view: 'a data t -> 'a data view
      val mk_Empty: 'a data t
      val mk_Lf: K.t -> 'a data -> 'a data t
      val mk_Br: int -> 'a data t -> 'a data t -> 'a data t
      val ktag : 'a data t -> int
    end)
  = struct
    open G

    type key = K.t
    type 'a data = 'a G.data
    type 'a t = 'a G.t

    (* ---------------------------------------------------------------------- *)
    (* --- Smart Constructors                                             --- *)
    (* ---------------------------------------------------------------------- *)

    let empty = mk_Empty
    let singleton = mk_Lf

    let br p t0 t1 = match view t0 , view t1 with
      | Empty,_ -> t1
      | _,Empty -> t0
      | _ -> mk_Br p t0 t1

    let lf k = function None -> mk_Empty | Some x -> mk_Lf k x

    (* good sharing *)
    let lf0 k x' t' = function
      | None -> mk_Empty
      | Some x -> if x == x' then t' else mk_Lf k x

    (* good sharing *)
    let br0 p t0' t1' t' t0 = match view t0 with
      | Empty -> t1'
      | _ -> if t0' == t0 then t' else mk_Br p t0 t1'

    (* good sharing *)
    let br1 p t0' t1' t' t1 = match view t1 with
      | Empty -> t0'
      | _ -> if t1' == t1 then t' else mk_Br p t0' t1

    let join p t0 q t1 =
      let m = branching_bit p q in
      let r = mask p m in
      if zero_bit p r
      then mk_Br r t0 t1
      else mk_Br r t1 t0

    let side p q = (* true this side, false inverse *)
      let m = branching_bit p q in
      let r = mask p m in
      zero_bit p r

    (* t0 and t1 has different prefix, but best common prefix is unknown *)
    let glue t0 t1 =
      match view t0 , view t1 with
      | Empty,_ -> t1
      | _,Empty -> t0
      | _,_ -> join (ktag t0) t0 (ktag t1) t1


    let glue' ~old ~cur ~other ~all =
      if old == cur then all else glue cur other

    let glue0 t0 t0' t1' t' =
      if t0 == t0' then t' else glue t0 t1'

    let glue1 t1 t0' t1' t' =
      if t1 == t1' then t' else glue t0' t1

    let glue01 t0 t1 t0' t1' t' =
      if t0 == t0' && t1 == t1' then t' else glue t0 t1

    let glue2 t0 t1 t0' t1' t' s0' s1' s' =
      if t0 == s0' && t1 == s1' then s' else
      if t0 == t0' && t1 == t1' then t' else glue t0 t1

    (* ---------------------------------------------------------------------- *)
    (* --- Access API                                                     --- *)
    (* ---------------------------------------------------------------------- *)

    let is_empty x = match view x with
      | Empty -> true
      | Lf _ | Br _ -> false

    let size t =
      let rec walk n t = match view t with
        | Empty -> n
        | Lf _ -> succ n
        | Br(_,a,b) -> walk (walk n a) b
      in walk 0 t

    let cardinal = size

    let rec mem k t = match view t with
      | Empty -> false
      | Lf(i,_) -> K.equal i k
      | Br(p,t0,t1) ->
        match_prefix (K.tag k) p &&
        mem k (if zero_bit (K.tag k) p then t0 else t1)

    let rec findq k t = match view t with
      | Empty -> raise Not_found
      | Lf(i,x) -> if K.equal k i then (x,t) else raise Not_found
      | Br(p,t0,t1) ->
        if match_prefix (K.tag k) p then
          findq k (if zero_bit (K.tag k) p then t0 else t1)
        else
          raise Not_found

    let rec find_exn exn k t = match view t with
      | Empty -> raise exn
      | Lf(i,x) -> if K.equal k i then x else raise exn
      | Br(p,t0,t1) ->
        if match_prefix (K.tag k) p then
          find_exn exn k (if zero_bit (K.tag k) p then t0 else t1)
        else
          raise exn

    let find k m = find_exn Not_found k m

    let rec find_opt k t = match view t with
      | Empty -> None
      | Lf(i,x) -> if K.equal k i then Some x else None
      | Br(p,t0,t1) ->
        if match_prefix (K.tag k) p then
          find_opt k (if zero_bit (K.tag k) p then t0 else t1)
        else
          None

    let rec find_def def k t = match view t with
      | Empty -> def
      | Lf(i,x) -> if K.equal k i then x else def
      | Br(p,t0,t1) ->
        if match_prefix (K.tag k) p then
          find_def def k (if zero_bit (K.tag k) p then t0 else t1)
        else
          def

    (* good sharing *)
    let rec find_remove k t = match view t with
      | Empty -> mk_Empty, None
      | Lf(i,y) ->
        if K.equal i k then
          mk_Empty, Some y
        else
          t, None
      | Br(p,t0,t1) ->
        if match_prefix (K.tag k) p then
          (* k belongs to tree *)
          if zero_bit (K.tag k) p
          then let t0', r = find_remove k t0 in
            br0 p t0 t1 t t0', r (* k is in t0 *)
          else let t1', r = find_remove k t1 in
            br1 p t0 t1 t t1', r (* k is in t1 *)
        else
          (* k is disjoint from tree *)
          t, None

    (** shouldn't be used at top *)
    let rec max_binding_opt t = match view t with
      | Empty -> None
      | Lf(k,x) -> Some(k,x)
      | Br(_,_,t1) -> max_binding_opt t1

    let rec find_smaller_opt' cand k t = match view t with
      | Empty -> assert false
      | Lf(i,y) ->
        let c = Pervasives.compare (K.tag i) (K.tag k) in
        if c <= 0 then Some(i,y)
        else (* c > 0 *) max_binding_opt cand
      | Br(p,t0,t1) ->
        if match_prefix (K.tag k) p then
          (* k belongs to tree *)
          if zero_bit (K.tag k) p
          then find_smaller_opt' cand k t0
          else find_smaller_opt' t0 k t1
        else
          (* k is disjoint from tree *)
        if side (K.tag k) p
        then (* k p *) max_binding_opt cand
        else (* p k *) max_binding_opt t1

    let find_smaller_opt k t = match view t with
      | Empty -> None
      | Br(p,t0,t1) when p = max_int ->
        (* k belongs to tree *)
        if zero_bit (K.tag k) p
        then find_smaller_opt' t1 k t0
        else find_smaller_opt' mk_Empty k t1
      | _ ->
        find_smaller_opt' mk_Empty k t

    (* ---------------------------------------------------------------------- *)
    (* --- Comparison                                                     --- *)
    (* ---------------------------------------------------------------------- *)

    let rec compare cmp s t =
      if (Obj.magic s) == t then 0 else
        match view s , view t with
        | Empty , Empty -> 0
        | Empty , _ -> (-1)
        | _ , Empty -> 1
        | Lf(i,x) , Lf(j,y) ->
          let ck = Pervasives.compare (K.tag i) (K.tag j) in
          if ck = 0 then cmp x y else ck
        | Lf _ , _ -> (-1)
        | _ , Lf _ -> 1
        | Br(p,s0,s1) , Br(q,t0,t1) ->
          let cp = Pervasives.compare p q in
          if cp <> 0 then cp else
            let c0 = compare cmp s0 t0 in
            if c0 <> 0 then c0 else
              compare cmp s1 t1

    let rec equal eq s t =
      if (Obj.magic s) == t then true else
        match view s , view t with
        | Empty , Empty -> true
        | Lf(i,x) , Lf(j,y) -> K.equal i j && eq x y
        | Br(p,s0,s1) , Br(q,t0,t1) ->
          p==q && equal eq s0 t0 && equal eq s1 t1
        | _ -> false

    (* ---------------------------------------------------------------------- *)
    (* --- Addition, Insert, Change, Remove                               --- *)
    (* ---------------------------------------------------------------------- *)

    (* good sharing *)
    let rec change phi k x t = match view t with
      | Empty -> (match phi k x None with
          | None -> t
          | Some w -> mk_Lf k w)
      | Lf(i,y) ->
        if K.equal i k then
          lf0 k y t (phi k x (Some y))
        else
          (match phi k x None with
           | None -> t
           | Some w -> let s = mk_Lf k w in
             join (K.tag k) s (K.tag i) t)
      | Br(p,t0,t1) ->
        if match_prefix (K.tag k) p then
          (* k belongs to tree *)
          if zero_bit (K.tag k) p
          then br0 p t0 t1 t (change phi k x t0) (* k is in t0 *)
          else br1 p t0 t1 t (change phi k x t1) (* k is in t1 *)
        else
          (* k is disjoint from tree *)
          (match phi k x None with
           | None -> t
           | Some w -> let s = mk_Lf k w in
             join (K.tag k) s p t)

    (* good sharing *)
    let insert f k x = change (fun _k x -> function
        | None -> Some x
        | Some old -> Some (f k x old)) k x

    (* good sharing *)
    let add k x m = change (fun _k x _old -> Some x) k x m

    (* good sharing *)
    let remove k m = change (fun _k () _old -> None) k () m

    (* good sharing *)
    let add_new e x v m = change (fun _k (e,v) -> function
        | Some _ -> raise e
        | None   -> Some v) x (e,v) m

    (* good sharing *)
    let rec add_change empty add k b t = match view t with
      | Empty -> mk_Lf k (empty b)
      | Lf(i,y) ->
        if K.equal i k then
          let y' = (add b y) in
          if y == y' then t else mk_Lf i y'
        else
          let s = mk_Lf k (empty b) in
          join (K.tag k) s (K.tag i) t
      | Br(p,t0,t1) ->
        if match_prefix (K.tag k) p then
          (* k belongs to tree *)
          if zero_bit (K.tag k) p
          then mk_Br p (add_change empty add k b t0) t1 (* k is in t0 *)
          else mk_Br p t0 (add_change empty add k b t1) (* k is in t1 *)
        else
          (* k is disjoint from tree *)
          let s = mk_Lf k (empty b) in
          join (K.tag k) s p t

    (* ---------------------------------------------------------------------- *)
    (* --- Map                                                            --- *)
    (* ---------------------------------------------------------------------- *)

    let mapi phi t =
      let rec mapi phi t = match view t with
        | Empty   -> mk_Empty
        | Lf(k,x) -> mk_Lf k (phi k x)
        | Br(p,t0,t1) ->
          let t0 = mapi phi t0 in
          let t1 = mapi phi t1 in
          mk_Br p t0 t1
      in match view t with (* in order to be sorted *)
      | Empty   -> mk_Empty
      | Lf(k,x) -> mk_Lf k (phi k x)
      | Br(p,t0,t1) when p = max_int -> let t1 = mapi phi t1 in
        let t0 = mapi phi t0 in mk_Br p t0 t1
      | Br(p,t0,t1)                  -> let t0 = mapi phi t0 in
        let t1 = mapi phi t1 in mk_Br p t0 t1
    let map phi = mapi (fun _ x -> phi x)

    let mapf phi t =
      let rec mapf phi t = match view t with
        | Empty   -> mk_Empty
        | Lf(k,x) -> lf k (phi k x)
        | Br(_,t0,t1) -> glue (mapf phi t0) (mapf phi t1)
      in match view t with (* in order to be sorted *)
      | Empty   -> mk_Empty
      | Lf(k,x) -> lf k (phi k x)
      | Br(p,t0,t1) when p = max_int -> let t1 = mapf phi t1 in
        let t0 = mapf phi t0 in glue t0 t1
      | Br(_,t0,t1)                  -> let t0 = mapf phi t0 in
        let t1 = mapf phi t1 in glue t0 t1

    (* good sharing *)
    let mapq phi t =
      let rec mapq phi t = match view t with
        | Empty -> t
        | Lf(k,x) -> lf0 k x t (phi k x)
        | Br(_,t0,t1) ->
          let t0' = mapq phi t0 in
          let t1' = mapq phi t1 in
          glue01 t0' t1' t0 t1 t
      in match view t with (* to be sorted *)
      | Empty -> t
      | Lf(k,x) -> lf0 k x t (phi k x)
      | Br(p,t0,t1) when p = max_int ->
        let t1' = mapq phi t1 in
        let t0' = mapq phi t0 in
        glue01 t0' t1' t0 t1 t
      | Br(_,t0,t1) ->
        let t0' = mapq phi t0 in
        let t1' = mapq phi t1 in
        glue01 t0' t1' t0 t1 t

    (** bad sharing but polymorph *)
    let mapq' :
      type a b. (key -> a data -> b data option) -> a data t -> b data t=
      fun phi t ->
        let rec aux phi t = match view t with
          | Empty -> mk_Empty
          | Lf(k,x) -> lf k (phi k x)
          | Br(_,t0,t1) ->
            let t0' = aux phi t0 in
            let t1' = aux phi t1 in
            glue t0' t1'
        in match view t with (* to be sorted *)
        | Empty -> mk_Empty
        | Lf(k,x) -> lf k (phi k x)
        | Br(p,t0,t1) when p = max_int ->
          let t1' = aux phi t1 in
          let t0' = aux phi t0 in
          glue t0' t1'
        | Br(_,t0,t1) ->
          let t0' = aux phi t0 in
          let t1' = aux phi t1 in
          glue t0' t1'

    let filter f m = mapq' (fun k v -> if f k v then Some v else None) m
    let mapi_filter = mapq'
    let map_filter f m = mapq' (fun _ v -> f v) m

    (*
       bad sharing because the input type can be differente of the
       output type it is possible but currently too many Obj.magic are
       needed in lf0 and glue01
 *)
    let mapi_filter_fold:
      type a b acc. (key -> a data -> acc -> acc * b data option) ->
      a data t -> acc -> acc * b data t
      = fun phi t acc ->
        let rec aux phi t acc = match view t with
          | Empty -> acc, mk_Empty
          | Lf(k,x) -> let acc,x = (phi k x acc) in acc, lf k x
          | Br(_,t0,t1) ->
            let acc, t0' = aux phi t0 acc in
            let acc, t1' = aux phi t1 acc in
            acc, glue t0' t1'
        in match view t with (* to be sorted *)
        | Empty -> acc, mk_Empty
        | Lf(k,x) -> let acc,x = (phi k x acc) in acc, lf k x
        | Br(p,t0,t1) when p = max_int ->
          let acc, t1' = aux phi t1 acc in
          let acc, t0' = aux phi t0 acc in
          acc, glue t0' t1'
        | Br(_,t0,t1) ->
          let acc, t0' = aux phi t0 acc in
          let acc, t1' = aux phi t1 acc in
          acc, glue t0' t1'

    let mapi_fold phi t acc =
      mapi_filter_fold (fun k v acc ->
          let acc, v' = phi k v acc in
          acc, Some v') t acc

    (* good sharing *)
    let rec partition p t = match view t with
      | Empty -> (t,t)
      | Lf(k,x) -> if p k x then t,mk_Empty else mk_Empty,t
      | Br(_,t0,t1) ->
        let (t0',u0') = partition p t0 in
        let (t1',u1') = partition p t1 in
        if t0'==t0 && t1'==t1 then (t, u0') (* u0' and u1' are empty *)
        else if u0'==t0 && u1'==t1 then (t0', t) (* t0' and t1' are empty *)
        else (glue t0' t1'),(glue u0' u1')

    (* good sharing *)
    let split k t =
      let rec aux k t = match view t with
        | Empty -> assert false (** absurd: only at top *)
        | Lf(k',x) -> let c = Pervasives.compare (K.tag k) (K.tag k') in
          if c = 0 then (mk_Empty,Some x,mk_Empty)
          else if c < 0 then (mk_Empty, None, t)
          else (* c > 0 *)   (t, None, mk_Empty)
        | Br(p,t0,t1) ->
          if match_prefix (K.tag k) p then
            if zero_bit (K.tag k) p
            then
              let (t0',r,t1') = aux k t0 in
              (t0',r,glue' ~old:t0 ~cur:t1' ~other:t1 ~all:t )
            else
              let (t0',r,t1') = aux k t1 in
              (glue' ~old:t1 ~cur:t0' ~other:t0 ~all:t,r,t1')
          else
          if side (K.tag k) p
          then (* k p *) (mk_Empty, None, t)
          else (* p k *) (t, None, mk_Empty)
      in match view t with
      | Empty -> mk_Empty, None, mk_Empty
      | Br(p,t0,t1) when p = max_int -> (** inverted *)
        if zero_bit (K.tag k) p
        then
          let (t0',r,t1') = aux k t0 in
          (glue' ~old:t0 ~cur:t0' ~other:t1 ~all:t,r,t1' )
        else
          let (t0',r,t1') = aux k t1 in
          (t0',r,glue' ~old:t1 ~cur:t1' ~other:t0 ~all:t)
      | _ -> aux k t

    (* good sharing *)
    let rec partition_split p t = match view t with
      | Empty -> (t,t)
      | Lf(k,x) -> let u,v = p k x in (lf0 k x t u), (lf0 k x t v)
      | Br(_,t0,t1) ->
        let t0',u0' = partition_split p t0 in
        let t1',u1' = partition_split p t1 in
        if t0'==t0 && t1'==t1 then (t, u0') (* u0' and u1' are empty *)
        else if u0'==t0 && u1'==t1 then (t0', t) (* t0' and t1' are empty *)
        else (glue t0' t1'),(glue u0' u1')

    (* ---------------------------------------------------------------------- *)
    (* --- Iter                                                           --- *)
    (* ---------------------------------------------------------------------- *)

    let iteri phi t =
      let rec aux t = match view t with
        | Empty -> ()
        | Lf(k,x) -> phi k x
        | Br(_,t0,t1) -> aux t0 ; aux t1
      in match view t with (* in order to be sorted *)
      | Empty -> ()
      | Lf(k,x) -> phi k x
      | Br(p,t0,t1) when p = max_int -> aux t1 ; aux t0
      | Br(_,t0,t1)                  -> aux t0 ; aux t1

    let iter = iteri

    let foldi phi t e = (* increasing order *)
      let rec aux t e = match view t with
        | Empty -> e
        | Lf(i,x) -> phi i x e
        | Br(_,t0,t1) -> aux t1 (aux t0 e)
      in match view t with (* to be sorted *)
      | Empty -> e
      | Lf(i,x) -> phi i x e
      | Br(p,t0,t1) when p = max_int -> aux t0 (aux t1 e)
      | Br(_,t0,t1)                  -> aux t1 (aux t0 e)

    let fold = foldi

    let fold_left phi e t = (* increasing order *)
      let rec aux t e = match view t with
        | Empty -> e
        | Lf(k,x) -> phi e k x
        | Br(_,t0,t1) -> aux t1 (aux t0 e)
      in match view t with (* to be sorted *)
      | Empty -> e
      | Lf(k,x) -> phi e k x
      | Br(p,t0,t1) when p = max_int -> aux t0 (aux t1 e)
      | Br(_,t0,t1)                  -> aux t1 (aux t0 e)

    let foldd phi e t = (* decreasing order *)
      let rec aux t e = match view t with
        | Empty -> e
        | Lf(i,x) -> phi e i x
        | Br(_,t0,t1) -> aux t0 (aux t1 e)
      in match view t with (* to be sorted *)
      | Empty -> e
      | Lf(i,x) -> phi e i x
      | Br(p,t0,t1) when p = max_int -> aux t1 (aux t0 e)
      | Br(_,t0,t1)                  -> aux t0 (aux t1 e)


    let fold_decr = foldd

    (* decreasing order on f to have the list in increasing order *)
    let mapl f m = foldd (fun a k v -> (f k v)::a) [] m
    let bindings m = mapl (fun k v -> (k,v)) m
    let values m =  mapl (fun _ v -> v) m
    let keys m =  mapl (fun k _ -> k) m

    let for_all phi t = (* increasing order *)
      let rec aux t = match view t with
        | Empty -> true
        | Lf(k,x) -> phi k x
        | Br(_,t0,t1) -> aux t0 && aux t1
      in match view t with (* in order to be sorted *)
      | Empty -> true
      | Lf(k,x) -> phi k x
      | Br(p,t0,t1) when p = max_int -> aux t1 && aux t0
      | Br(_,t0,t1)                  -> aux t0 && aux t1

    let exists phi t = (* increasing order *)
      let rec aux t = match view t with
        | Empty -> false
        | Lf(k,x) -> phi k x
        | Br(_,t0,t1) -> aux t0 || aux t1
      in match view t with (* in order to be sorted *)
      | Empty -> false
      | Lf(k,x) -> phi k x
      | Br(p,t0,t1) when p = max_int -> aux t1 || aux t0
      | Br(_,t0,t1)                  -> aux t0 || aux t1


    let min_binding t = (* increasing order *)
      let rec aux t = match view t with
        | Empty -> assert false (** absurd: only at top *)
        | Lf(k,x) -> (k,x)
        | Br(_,t0,_) -> aux t0
      in match view t with (* in order to be sorted *)
      | Empty -> raise Not_found
      | Lf(k,x) -> (k,x)
      | Br(p,_,t1) when p = max_int -> aux t1
      | Br(_,t0,_)                  -> aux t0

    let max_binding t = (* increasing order *)
      let rec aux t = match view t with
        | Empty -> assert false (** absurd: only at top *)
        | Lf(k,x) -> (k,x)
        | Br(_,_,t1) -> aux t1
      in match view t with (* in order to be sorted *)
      | Empty -> raise Not_found
      | Lf(k,x) -> (k,x)
      | Br(p,t0,_) when p = max_int -> aux t0
      | Br(_,_,t1)                  -> aux t1

    let choose = min_binding

    (* ---------------------------------------------------------------------- *)
    (* --- Inter                                                          --- *)
    (* ---------------------------------------------------------------------- *)

    let occur i t = try Some (find i t) with Not_found -> None

    let rec interi lf_phi s t =
      match view s , view t with
      | Empty , _ -> mk_Empty
      | _ , Empty -> mk_Empty
      | Lf(i,x) , Lf(j,y) ->
        if K.equal i j
        then lf_phi i x y
        else mk_Empty
      | Lf(i,x) , Br _ ->
        (match occur i t with None -> mk_Empty | Some y -> lf_phi i x y)
      | Br _ , Lf(j,y) ->
        (match occur j s with None -> mk_Empty | Some x -> lf_phi j x y)
      | Br(p,s0,s1) , Br(q,t0,t1) ->
        if p == q then
          (* prefixes agree *)
          glue (interi lf_phi s0 t0) (interi lf_phi s1 t1)
        else if included_prefix p q then
          (* q contains p. Intersect t with a subtree of s *)
          if zero_bit q p
          then interi lf_phi s0 t (* t has bit m = 0 => t is inside s0 *)
          else interi lf_phi s1 t (* t has bit m = 1 => t is inside s1 *)
        else if included_prefix q p then
          (* p contains q. Intersect s with a subtree of t *)
          if zero_bit p q
          then interi lf_phi s t0 (* s has bit n = 0 => s is inside t0 *)
          else interi lf_phi s t1 (* t has bit n = 1 => s is inside t1 *)
        else
          (* prefix disagree *)
          mk_Empty

    let interf phi = interi (fun i x y -> mk_Lf i (phi i x y))
    let inter phi = interi (fun i x y -> lf i (phi i x y))

    (* good sharing with s  *)
    let lfq phi i x y s t =
      match phi i x y with
      | None -> mk_Empty
      | Some w -> if w == x then s else if w == y then t else mk_Lf i w
    let occur0 phi i x s t =
      try let (y,t) = findq i t in lfq phi i x y s t
      with Not_found -> mk_Empty
    let occur1 phi j y s t =
      try let (x,s) = findq j s in lfq phi j x y s t
      with Not_found -> mk_Empty

    (* good sharing with s *)
    let rec interq phi s t =
      match view s , view t with
      | Empty , _ -> s
      | _ , Empty -> t
      | Lf(i,x) , Lf(j,y) ->
        if K.equal i j
        then lfq phi i x y s t
        else mk_Empty
      | Lf(i,x) , Br _ -> occur0 phi i x s t
      | Br _ , Lf(j,y) -> occur1 phi j y s t
      | Br(p,s0,s1) , Br(q,t0,t1) ->
        if p == q then
          (* prefixes agree *)
          glue2 (interq phi s0 t0) (interq phi s1 t1) s0 s1 s t0 t1 t
        else if included_prefix p q then
          (* q contains p. Intersect t with a subtree of s *)
          if zero_bit q p
          then interq phi s0 t (* t has bit m = 0 => t is inside s0 *)
          else interq phi s1 t (* t has bit m = 1 => t is inside s1 *)
        else if included_prefix q p then
          (* p contains q. Intersect s with a subtree of t *)
          if zero_bit p q
          then interq phi s t0 (* s has bit n = 0 => s is inside t0 *)
          else interq phi s t1 (* t has bit n = 1 => s is inside t1 *)
        else
          (* prefix disagree *)
          mk_Empty

    (* ---------------------------------------------------------------------- *)
    (* --- Union                                                          --- *)
    (* ---------------------------------------------------------------------- *)

    (* good sharing with s *)
    let br2u p s0' s1' s' t0' t1' t' t0 t1=
      if s0'==t0 && s1'== t1 then s' else
      if t0'==t0 && t1'== t1 then t' else
        mk_Br p t0 t1

    (* good sharing with s *)
    let br0u p t0' t1' t' t0 = if t0'==t0 then t' else mk_Br p t0 t1'
    let br1u p t0' t1' t' t1 = if t1'==t1 then t' else mk_Br p t0' t1

    (* good sharing with s *)
    let rec unionf phi s t =
      match view s , view t with
      | Empty , _ -> t
      | _ , Empty -> s
      | Lf(i,x) , Lf(j,y) ->
        if K.equal i j
        then let w = phi i x y in
          if w == x then s else if w == y then t else mk_Lf i w
        else join (K.tag i) s (K.tag j) t
      | Lf(i,x) , Br _ -> insert phi i x t
      | Br _ , Lf(j,y) -> insert (fun j y x -> phi j x y) j y s
      | Br(p,s0,s1) , Br(q,t0,t1) ->
        if p == q then
          (* prefixes agree *)
          br2u p s0 s1 s t0 t1 t (unionf phi s0 t0) (unionf phi s1 t1)
        else if included_prefix p q then
          (* q contains p. Merge t with a subtree of s *)
          if zero_bit q p
          then
            (* t has bit m = 0 => t is inside s0 *)
            br0u p s0 s1 s (unionf phi s0 t)
          else
            (* t has bit m = 1 => t is inside s1 *)
            br1u p s0 s1 s (unionf phi s1 t)
        else if included_prefix q p then
          (* p contains q. Merge s with a subtree of t *)
          if zero_bit p q
          then
            (* s has bit n = 0 => s is inside t0 *)
            br0u q t0 t1 t (unionf phi s t0)
          else
            (* t has bit n = 1 => s is inside t1 *)
            br1u q t0 t1 t (unionf phi s t1)
        else
          (* prefix disagree *)
          join p s q t

    (* good sharing with s *)
    let rec union phi s t =
      match view s , view t with
      | Empty , _ -> t
      | _ , Empty -> s
      | Lf(i,x) , Lf(j,y) ->
        if K.equal i j
        then match phi i x y with
          | Some w when w == x -> s
          | Some w when w == y -> t
          | Some w             -> mk_Lf i w
          | None               -> mk_Empty
        else join (K.tag i) s (K.tag j) t
      | Lf(i,x) , Br _ ->
        change (fun i x -> function | None -> Some x
                                    | Some old -> (phi i x old)) i x t
      | Br _ , Lf(j,y) ->
        change (fun j y -> function | None -> Some y
                                    | Some old -> (phi j old y)) j y s
      | Br(p,s0,s1) , Br(q,t0,t1) ->
        if p == q then
          (* prefixes agree *)
          glue2 (union phi s0 t0) (union phi s1 t1) s0 s1 s t0 t1 t
        else if included_prefix p q then
          (* q contains p. Merge t with a subtree of s *)
          if zero_bit q p
          then
            (* t has bit m = 0 => t is inside s0 *)
            br0 p s0 s1 s (union phi s0 t)
          else
            (* t has bit m = 1 => t is inside s1 *)
            br1 p s0 s1 s (union phi s1 t)
        else if included_prefix q p then
          (* p contains q. Merge s with a subtree of t *)
          if zero_bit p q
          then
            (* s has bit n = 0 => s is inside t0 *)
            br0 q t0 t1 t (union phi s t0)
          else
            (* t has bit n = 1 => s is inside t1 *)
            br1 q t0 t1 t (union phi s t1)
        else
          (* prefix disagree *)
          join p s q t

    (* ---------------------------------------------------------------------- *)
    (* --- Merge                                                          --- *)
    (* ---------------------------------------------------------------------- *)

    let map1 phi s = mapf (fun i x -> phi i (Some x) None) s
    let map2 phi t = mapf (fun j y -> phi j None (Some y)) t

    let rec merge phi s t =
      match view s , view t with
      | Empty , _ -> map2 phi t
      | _ , Empty -> map1 phi s
      | Lf(i,x) , Lf(j,y) ->
        if K.equal i j then lf i (phi i (Some x) (Some y))
        else
          let a = lf i (phi i (Some x) None) in
          let b = lf j (phi j None (Some y)) in
          glue a b

      | Lf(i,x) , Br(q,t0,t1) ->
        if match_prefix (K.tag i) q then
          (* leaf i is in tree t *)
          if zero_bit (K.tag i) q
          then glue (merge phi s t0) (map2 phi t1) (* s=i is in t0 *)
          else glue (map2 phi t0) (merge phi s t1) (* s=i is in t1 *)
        else
          (* leaf i does not appear in t *)
          glue (lf i (phi i (Some x) None)) (map2 phi t)

      | Br(p,s0,s1) , Lf(j,y) ->
        if match_prefix (K.tag j) p then
          (* leaf j is in tree s *)
          if zero_bit (K.tag j) p
          then glue (merge phi s0 t) (map1 phi s1) (* t=j is in s0 *)
          else glue (map1 phi s0) (merge phi s1 t) (* t=j is in s1 *)
        else
          (* leaf j does not appear in s *)
          glue (map1 phi s) (lf j (phi j None (Some y)))

      | Br(p,s0,s1) , Br(q,t0,t1) ->
        if p == q then
          (* prefixes agree *)
          glue (merge phi s0 t0) (merge phi s1 t1)
        else if included_prefix p q then
          (* q contains p. Merge t with a subtree of s *)
          if zero_bit q p
          then (* t has bit m = 0 => t is inside s0 *)
            glue (merge phi s0 t) (map1 phi s1)
          else (* t has bit m = 1 => t is inside s1 *)
            glue (map1 phi s0) (merge phi s1 t)
        else if included_prefix q p then
          (* p contains q. Merge s with a subtree of t *)
          if zero_bit p q
          then (* s has bit n = 0 => s is inside t0 *)
            glue (merge phi s t0) (map2 phi t1)
          else (* s has bit n = 1 => s is inside t1 *)
            glue (map2 phi t0) (merge phi s t1)
        else
          glue (map1 phi s) (map2 phi t)

    let map2 phi t = mapf (fun j y -> phi j None y) t
    let rec union_merge phi s t =
      match view s , view t with
      | Empty , _ -> map2 phi t
      | _ , Empty -> s
      | Lf(i,x) , Lf(j,y) ->
        if K.equal i j then lf i (phi i (Some x) y)
        else
          let b = lf j (phi j None y) in
          glue s b

      | Lf(i,x) , Br(q,t0,t1) ->
        if match_prefix (K.tag i) q then
          (* leaf i is in tree t *)
          if zero_bit (K.tag i) q
          then glue (union_merge phi s t0) (map2 phi t1) (* s=i is in t0 *)
          else glue (map2 phi t0) (union_merge phi s t1) (* s=i is in t1 *)
        else
          (* leaf i does not appear in t *)
          glue s (map2 phi t)

      | Br(p,s0,s1) , Lf(j,y) ->
        if match_prefix (K.tag j) p then
          (* leaf j is in tree s *)
          if zero_bit (K.tag j) p
          then glue (union_merge phi s0 t) s1 (* t=j is in s0 *)
          else glue s0 (union_merge phi s1 t) (* t=j is in s1 *)
        else
          (* leaf j does not appear in s *)
          glue s (lf j (phi j None y))

      | Br(p,s0,s1) , Br(q,t0,t1) ->
        if p == q then
          (* prefixes agree *)
          glue (union_merge phi s0 t0) (union_merge phi s1 t1)
        else if included_prefix p q then
          (* q contains p. Merge t with a subtree of s *)
          if zero_bit q p
          then (* t has bit m = 0 => t is inside s0 *)
            glue (union_merge phi s0 t) s1
          else (* t has bit m = 1 => t is inside s1 *)
            glue s0 (union_merge phi s1 t)
        else if included_prefix q p then
          (* p contains q. Merge s with a subtree of t *)
          if zero_bit p q
          then (* s has bit n = 0 => s is inside t0 *)
            glue (union_merge phi s t0) (map2 phi t1)
          else (* s has bit n = 1 => s is inside t1 *)
            glue (map2 phi t0) (union_merge phi s t1)
        else
          glue s (map2 phi t)



    (* good sharing with s *)
    let rec diffq phi s t =
      match view s , view t with
      | Empty , _ -> s
      | _ , Empty -> s
      | Lf(i,x) , Lf(j,y) ->
        if K.equal i j
        then lfq phi i x y s t
        else s
      | Lf(i,x) , Br _ ->
        (match occur i t with None -> s | Some y -> lfq phi i x y s t)
      | Br _ , Lf(j,y) -> change (fun j y x -> match x with
          | None -> None
          | Some x -> phi j x y) j y s
      | Br(p,s0,s1) , Br(q,t0,t1) ->
        if p == q then
          (* prefixes agree *)
          let t0' = (diffq phi s0 t0) in
          let t1' = (diffq phi s1 t1) in
          glue01 t0' t1' s0 s1 s
        else if included_prefix p q then
          (* q contains p. *)
          if zero_bit q p
          then (* t has bit m = 0 => t is inside s0 *)
            let s0' = (diffq phi s0 t) in
            glue0 s0' s0 s1 s
          else (* t has bit m = 1 => t is inside s1 *)
            let s1' = (diffq phi s1 t) in
            glue1 s1' s0 s1 s
        else if included_prefix q p then
          (* p contains q. *)
          if zero_bit p q
          then diffq phi s t0 (* s has bit n = 0 => s is inside t0 *)
          else diffq phi s t1 (* t has bit n = 1 => s is inside t1 *)
        else
          (* prefix disagree *)
          s

    (* good sharing with s *)
    let rec diff :
      type a b. (key -> a data -> b data -> a data option) ->
      a data t -> b data t -> a data t
      = fun phi s t ->
        match view s , view t with
        | Empty , _ -> s
        | _ , Empty -> s
        | Lf(i,x) , Lf(j,y) ->
          if K.equal i j
          then lf0 i x s (phi i x y)
          else s
        | Lf(i,x) , Br _ ->
          (match occur i t with None -> s | Some y -> lf0 i x s (phi i x y))
        | Br _ , Lf(j,y) -> change (fun j y x -> match x with
            | None -> None
            | Some x -> phi j x y) j y s
        | Br(p,s0,s1) , Br(q,t0,t1) ->
          if p == q then
            (* prefixes agree *)
            let t0' = (diff phi s0 t0) in
            let t1' = (diff phi s1 t1) in
            glue01 t0' t1' s0 s1 s
          else if included_prefix p q then
            (* q contains p. *)
            if zero_bit q p
            then (* t has bit m = 0 => t is inside s0 *)
              let s0' = (diff phi s0 t) in
              glue0 s0' s0 s1 s
            else (* t has bit m = 1 => t is inside s1 *)
              let s1' = (diff phi s1 t) in
              glue1 s1' s0 s1 s
          else if included_prefix q p then
            (* p contains q. *)
            if zero_bit p q
            then diff phi s t0 (* s has bit n = 0 => s is inside t0 *)
            else diff phi s t1 (* t has bit n = 1 => s is inside t1 *)
          else
            (* prefix disagree *)
            s

    (* ---------------------------------------------------------------------- *)
    (* --- Iter Kernel                                                    --- *)
    (* ---------------------------------------------------------------------- *)

    let rec iterk phi s t =
      match view s , view t with
      | Empty , _ | _ , Empty -> ()
      | Lf(i,x) , Lf(j,y) -> if K.equal i j then phi i x y
      | Lf(i,x) , Br _ ->
        (match occur i t with None -> () | Some y -> phi i x y)
      | Br _ , Lf(j,y) ->
        (match occur j s with None -> () | Some x -> phi j x y)
      | Br(p,s0,s1) , Br(q,t0,t1) ->
        if p == q then
          (* prefixes agree *)
          (iterk phi s0 t0 ; iterk phi s1 t1)
        else if included_prefix p q then
          (* q contains p. Intersect t with a subtree of s *)
          if zero_bit q p
          then iterk phi s0 t (* t has bit m = 0 => t is inside s0 *)
          else iterk phi s1 t (* t has bit m = 1 => t is inside s1 *)
        else if included_prefix q p then
          (* p contains q. Intersect s with a subtree of t *)
          if zero_bit p q
          then iterk phi s t0 (* s has bit n = 0 => s is inside t0 *)
          else iterk phi s t1 (* t has bit n = 1 => s is inside t1 *)
        else
          (* prefix disagree *)
          ()

    (* ---------------------------------------------------------------------- *)
    (* --- Iter2                                                          --- *)
    (* ---------------------------------------------------------------------- *)

    let iter21 phi s = iteri (fun i x -> phi i (Some x) None) s
    let iter22 phi t = iteri (fun j y -> phi j None (Some y)) t

    let rec iter2 phi s t =
      match view s , view t with
      | Empty , _ -> iter22 phi t
      | _ , Empty -> iter21 phi s
      | Lf(i,x) , Lf(j,y) ->
        if K.equal i j then phi i (Some x) (Some y)
        else ( phi i (Some x) None ; phi j None (Some y) )

      | Lf(i,x) , Br(q,t0,t1) ->
        if match_prefix (K.tag i) q then
          (* leaf i is in tree t *)
          if zero_bit (K.tag i) q
          then (iter2 phi s t0 ; iter22 phi t1) (* s=i is in t0 *)
          else (iter22 phi t0 ; iter2 phi s t1) (* s=i is in t1 *)
        else
          (* leaf i does not appear in t *)
          (phi i (Some x) None ; iter22 phi t)

      | Br(p,s0,s1) , Lf(j,y) ->
        if match_prefix (K.tag j) p then
          (* leaf j is in tree s *)
          if zero_bit (K.tag j) p
          then (iter2 phi s0 t ; iter21 phi s1) (* t=j is in s0 *)
          else (iter21 phi s0 ; iter2 phi s1 t) (* t=j is in s1 *)
        else
          (* leaf j does not appear in s *)
          (iter21 phi s ; phi j None (Some y))

      | Br(p,s0,s1) , Br(q,t0,t1) ->
        if p == q then
          (* prefixes agree *)
          (iter2 phi s0 t0 ; iter2 phi s1 t1)
        else if included_prefix p q then
          (* q contains p. Merge t with a subtree of s *)
          if zero_bit q p
          then (* t has bit m = 0 => t is inside s0 *)
            (iter2 phi s0 t ; iter21 phi s1)
          else (* t has bit m = 1 => t is inside s1 *)
            (iter21 phi s0 ; iter2 phi s1 t)
        else if included_prefix q p then
          (* p contains q. Merge s with a subtree of t *)
          if zero_bit p q
          then (* s has bit n = 0 => s is inside t0 *)
            (iter2 phi s t0 ; iter22 phi t1)
          else (* s has bit n = 1 => s is inside t1 *)
            (iter22 phi t0 ; iter2 phi s t1)
        else
          (iter21 phi s ; iter22 phi t)

    (* ---------------------------------------------------------------------- *)
    (* --- Intersects                                                     --- *)
    (* ---------------------------------------------------------------------- *)

    (** TODO seems wrong *)
    let rec intersectf phi s t =
      match view s , view t with
      | Empty , _ -> false
      | _ , Empty -> false
      | Lf(i,x) , Lf(j,y) -> if K.equal i j then phi i x y else false
      | Lf(i,x) , Br _ -> (match occur i t with None -> false
                                              | Some y -> phi i x y)
      | Br _ , Lf(j,y) -> (match occur j s with None -> false
                                              | Some x -> phi j x y)
      | Br(p,s0,s1) , Br(q,t0,t1) ->
        if p == q then
          (* prefixes agree *)
          (intersectf phi s0 t0) || (intersectf phi s1 t1)
        else if included_prefix p q then
          (* q contains p. Intersect t with a subtree of s *)
          if zero_bit q p
          then intersectf phi s0 t (* t has bit m = 0 => t is inside s0 *)
          else intersectf phi s1 t (* t has bit m = 1 => t is inside s1 *)
        else if included_prefix q p then
          (* p contains q. Intersect s with a subtree of t *)
          if zero_bit p q
          then intersectf phi s t0 (* s has bit n = 0 => s is inside t0 *)
          else intersectf phi s t1 (* t has bit n = 1 => s is inside t1 *)
        else
          (* prefix disagree *)
          false

    let rec disjoint phi s t =
      match view s , view t with
      | Empty , _ -> true
      | _ , Empty -> true
      | Lf(i,x) , Lf(j,y) -> if K.equal i j then phi i x y else true
      | Lf(i,x) , Br _ -> (match occur i t with None -> true
                                              | Some y -> phi i x y)
      | Br _ , Lf(j,y) -> (match occur j s with None -> true
                                              | Some x -> phi j x y)
      | Br(p,s0,s1) , Br(q,t0,t1) ->
        if p == q then
          (* prefixes agree *)
          (disjoint phi s0 t0) && (disjoint phi s1 t1)
        else if included_prefix p q then
          (* q contains p. Intersect t with a subtree of s *)
          if zero_bit q p
          then disjoint phi s0 t (* t has bit m = 0 => t is inside s0 *)
          else disjoint phi s1 t (* t has bit m = 1 => t is inside s1 *)
        else if included_prefix q p then
          (* p contains q. Intersect s with a subtree of t *)
          if zero_bit p q
          then disjoint phi s t0 (* s has bit n = 0 => s is inside t0 *)
          else disjoint phi s t1 (* t has bit n = 1 => s is inside t1 *)
        else
          (* prefix disagree *)
          true

    (** fold2 *)
    let fold21 phi m acc = fold (fun i x acc -> phi i (Some x) None acc) m acc
    let fold22 phi m acc = fold (fun j y acc -> phi j None (Some y) acc) m acc

    (* good sharing with s *)
    let rec fold2_union:
      type a b c.
      (key -> a data option -> b data option -> c -> c) ->
      a data t -> b data t -> c -> c
      = fun phi s t acc ->
        match view s , view t with
        | Empty , _ -> fold22 phi t acc
        | _ , Empty -> fold21 phi s acc
        | Lf(i,x) , Lf(j,y) ->
          let c = Pervasives.compare (K.tag i) (K.tag j) in
          if c = 0
          then phi i (Some x) (Some y) acc
          else if c < 0 then phi j None (Some y) (phi i (Some x) None acc)
          else (* c > 0 *)   phi i (Some x) None (phi j None (Some y) acc)
        | Lf(k,x) , Br(p,t1,t2) ->
          if match_prefix (K.tag k) p then
            if zero_bit (K.tag k) p
            then fold22 phi t2 (fold2_union phi s t1 acc)
            else fold2_union phi s t2 (fold22 phi t1 acc)
          else
          if side (K.tag k) p
          then (* k p *) fold22 phi t (phi k (Some x) None acc)
          else (* p k *) phi k (Some x) None (fold22 phi t acc)
        | Br(p,s1,s2) , Lf(k,y) ->
          if match_prefix (K.tag k) p then
            if zero_bit (K.tag k) p
            then fold21 phi s2 (fold2_union phi s1 t acc)
            else fold2_union phi s2 t (fold21 phi s1 acc)
          else
          if side (K.tag k) p
          then (* k p *) fold21 phi s (phi k None (Some y) acc)
          else (* p k *) phi k None (Some y) (fold21 phi s acc)
        | Br(p,s0,s1) , Br(q,t0,t1) ->
          if p == q then
            (* prefixes agree *)
            fold2_union phi s1 t1 (fold2_union phi s0 t0 acc)
          else if included_prefix p q then
            (* q contains p. Merge t with a subtree of s *)
            if zero_bit q p
            then
              (* t has bit m = 0 => t is inside s0 *)
              fold21 phi s1 (fold2_union phi s0 t acc)
            else
              (* t has bit m = 1 => t is inside s1 *)
              fold2_union phi s1 t (fold21 phi s0 acc)
          else if included_prefix q p then
            (* p contains q. Merge s with a subtree of t *)
            if zero_bit p q
            then
              (* s has bit n = 0 => s is inside t0 *)
              fold22 phi t1 (fold2_union phi s t0 acc)
            else
              (* t has bit n = 1 => s is inside t1 *)
              fold2_union phi s t1 (fold22 phi t0 acc)
          else
            (* prefix disagree *)
          if side p q
          then (* p q *) fold22 phi t (fold21 phi s acc)
          else (* q p *) fold21 phi s (fold22 phi t acc)

    (* good sharing with s *)
    let rec fold2_inter phi s t acc =
      match view s , view t with
      | Empty , _ -> acc
      | _ , Empty -> acc
      | Lf(i,x) , Lf(j,y) ->
        if K.equal i j
        then phi i x y acc
        else acc
      | Lf(k,x) , Br _ ->
        begin match find_opt k t with
          | Some y -> phi k x y acc
          | None -> acc
        end
      | Br _ , Lf(k,y) ->
        begin match find_opt k s with
          | Some x -> phi k x y acc
          | None -> acc
        end
      | Br(p,s0,s1) , Br(q,t0,t1) ->
        if p == q then
          (* prefixes agree *)
          fold2_inter phi s1 t1 (fold2_inter phi s0 t0 acc)
        else if included_prefix p q then
          (* q contains p. Merge t with a subtree of s *)
          if zero_bit q p
          then
            (* t has bit m = 0 => t is inside s0 *)
            fold2_inter phi s0 t acc
          else
            (* t has bit m = 1 => t is inside s1 *)
            fold2_inter phi s1 t acc
        else if included_prefix q p then
          (* p contains q. Merge s with a subtree of t *)
          if zero_bit p q
          then
            (* s has bit n = 0 => s is inside t0 *)
            fold2_inter phi s t0 acc
          else
            (* t has bit n = 1 => s is inside t1 *)
            fold2_inter phi s t1 acc
        else
          (* prefix disagree *)
          acc

    (* ---------------------------------------------------------------------- *)
    (* --- Subset                                                         --- *)
    (* ---------------------------------------------------------------------- *)

    let rec subsetf phi s t =
      match view s , view t with
      | Empty , _ -> true
      | _ , Empty -> false
      | Lf(i,x) , Lf(j,y) -> if K.equal i j then phi i x y else false
      | Lf(i,x) , Br _ ->
        (match occur i t with None -> false | Some y -> phi i x y)
      | Br _ , Lf _ -> false
      | Br(p,s0,s1) , Br(q,t0,t1) ->
        if p == q then
          (* prefixes agree *)
          (subsetf phi s0 t0 && subsetf phi s1 t1)
        else if included_prefix p q then
          (* q contains p: t is included in a (strict) subtree of s *)
          false
        else if included_prefix q p then
          (* p contains q: s is included in a subtree of t *)
          if zero_bit p q
          then subsetf phi s t0 (* s has bit n = 0 => s is inside t0 *)
          else subsetf phi s t1 (* t has bit n = 1 => s is inside t1 *)
        else
          (* prefix disagree *)
          false

    let subset = subsetf
    let subsetk s t = subsetf (fun _i _x _y -> true) s t
    let submap = subsetf

    (* ---------------------------------------------------------------------- *)

    let rec _pp_tree tab fmt t =
      match view t with
      | Empty -> ()
      | Lf(k,_) ->
        let k = K.tag k in
        Format.fprintf fmt "%sL%a=%d" tab pp_bits k k
      | Br(p,l,r) ->
        let next = tab ^ "   " in
        _pp_tree next fmt l ;
        Format.fprintf fmt "%s@@%a" tab (pp_mask (decode_mask p)) p ;
        _pp_tree next fmt r


    let is_num_elt n m =
      try
        fold (fun _ _ n -> if n < 0 then raise Exit else n-1) m n = 0
      with Exit -> false

    let of_list l =
      List.fold_left (fun acc (k,d) -> add k d acc) empty l

    let translate f m =
      fold (fun k v acc -> add (f k) v acc) m empty

    (** set_* *)

    let set_union m1 m2 = unionf (fun _ x _ -> x) m1 m2
    let set_inter m1 m2 = interf (fun _ x _ -> x) m1 m2
    let set_diff m1 m2 = diff (fun _ _ _ -> None) m1 m2
    let set_submap m1 m2 = submap (fun _ _ _ -> true) m1 m2
    let set_disjoint m1 m2 = disjoint (fun _ _ _ -> false) m1 m2
    let set_compare m1 m2 = compare (fun _ _ -> 0) m1 m2
    let set_equal m1 m2 = equal (fun _ _ -> true) m1 m2

    (** the goal is to choose randomly but often the same than [choose] *)
    let choose_rnd f m =
      let rec aux f m ret =
        match view m with
        | Empty -> ()
        | Lf(k,v) -> if f () then (ret := (k,v); raise Exit)
        | Br(_,t1,t2) ->
          aux f t1 ret; aux f t2 ret
      in
      let ret = ref (Obj.magic 0) in
      try
        begin match view m with (* in order to be sorted *)
          | Empty -> raise Not_found
          | Br(p,_,t1) when p = max_int -> aux f t1 ret
          | _                           -> aux f m ret
        end;
        choose m
      with Exit -> !ret

    (** Enumeration *)
    type 'a enumeration =
      | EEmpty
      | ELf of K.t * 'a * 'a enum2

    and 'a enum2 =
      | End
      | EBr of 'a t * 'a enum2

    let rec cons_enum m e =
      match view m with
      | Empty -> assert false (** absurd: Empty can appear only a toplevel *)
      | Lf(i,x) -> ELf(i,x,e)
      | Br(_,t1,t2) -> cons_enum t1 (EBr(t2,e))

    let start_enum m =  (* in order to be sorted *)
      match view m with
      | Empty -> EEmpty
      | Lf(i,x) -> ELf(i,x,End)
      | Br(p,t1,t2) when p = max_int -> cons_enum t2 (EBr(t1, End))
      | Br(_,t1,t2) -> cons_enum t1 (EBr(t2, End))

    let val_enum = function
      | EEmpty -> None
      | ELf(i,x,_) -> Some (i,x)

    let next_enum_br = function
      | End -> EEmpty
      | EBr(t2,e) -> cons_enum t2 e

    let next_enum = function
      | EEmpty -> EEmpty
      | ELf(_,_,e) -> next_enum_br e

    let rec cons_ge_enum k m e =
      match view m with
      | Empty -> assert false (** absurd: Empty can appear only a toplevel *)
      | Lf(i,x) ->
        if side (K.tag i) (K.tag k)
        then (* i k *) next_enum_br e
        else (* k i *) ELf(i,x,e)
      | Br(p,t1,t2) ->
        if match_prefix (K.tag k) p then
          if zero_bit (K.tag k) p
          then cons_ge_enum k t1 (EBr(t2,e))
          else cons_ge_enum k t2 e
        else
        if side (K.tag k) p
        then (* k p *) cons_enum t1 (EBr(t2,e))
        else (* p k *) next_enum_br e

    let start_ge_enum k m =
      match view m with
      | Empty -> EEmpty
      | Br(p,t1,t2)  when p = max_int ->
        if zero_bit (K.tag k) p
        then cons_ge_enum k t1 End
        else cons_ge_enum k t2 (EBr(t1,End))
      | _ -> cons_ge_enum k m End

    let rec next_ge_enum_br k = function
      | End -> EEmpty
      | EBr(t,e) -> match view t with
        | Empty -> assert false (** absurd: Empty only at top *)
        | Lf(i,d) when (K.tag k) <= (K.tag i) -> ELf(i,d,e)
        | Lf(_,_) -> next_ge_enum_br k e
        | Br(p,t1,t2) ->
          if match_prefix (K.tag k) p then
            if zero_bit (K.tag k) p
            then cons_ge_enum k t1 (EBr(t2,e))
            else cons_ge_enum k t2 e
          else
          if side (K.tag k) p
          then (* k p *) cons_enum t1 (EBr(t2,e))
          else (* p k *) next_ge_enum_br k e

    let next_ge_enum k = function
      | EEmpty -> EEmpty
      | ELf(i,_,_) as e when (K.tag k) <= (K.tag i)-> e
      | ELf(_,_,e) -> next_ge_enum_br k e

    let change f k m = change (fun _ () v -> f v) k () m

    let add_opt x o m =
      match o with
      | None -> remove x m
      | Some y -> add x y m

    (** TODO more checks? *)
    let check_invariant m =
      match view m with
      | Empty -> true
      | _ ->
        let rec aux m =
          match view m with
          | Empty -> false
          | Lf _ -> true
          | Br (_,t1,t2) -> aux t1 && aux t2 in
        aux m

  let pp pp fmt m =
    Pp.iter2 iter Pp.arrow Pp.colon
      K.pp pp fmt m

  end

  module Def = struct
    type 'a t =
      | Empty
      | Lf of K.t * 'a
      | Br of int * 'a t * 'a t
  end

  module NT = struct

    module M : sig
      type 'a t = 'a Def.t
      type 'a data = 'a
      type 'a view = private
        | Empty
        | Lf of K.t * 'a
        | Br of int * 'a t * 'a t
      val view: 'a data t -> 'a data view
      val ktag : 'a data t -> int
      val mk_Empty: 'a data t
      val mk_Lf: K.t -> 'a data -> 'a data t
      val mk_Br: int -> 'a data t -> 'a data t -> 'a data t
    end = struct
      type 'a t = 'a Def.t
      type 'a data = 'a

      let ktag = function
        | Def.Empty -> assert false (** absurd: precondition: not Empty *)
        | Def.Lf(i,_) -> K.tag i
        | Def.Br(i,_,_) -> i
      let mk_Empty = Def.Empty
      let mk_Lf k d = Def.Lf(k,d)
      let mk_Br i t1 t2 =
        (* assert (t1 != Def.Empty && t2 != Def.Empty); *)
        Def.Br(i,t1,t2)

      type 'a view = 'a Def.t =
        | Empty
        | Lf of K.t * 'a
        | Br of int * 'a t * 'a t

      let view x = x

    end

    include Gen(M)

  end

  module Make(Data: Map_intf.HashType) :
    Map_intf.Map_hashcons with type 'a data = Data.t
                           and type 'a poly := 'a NT.t
                           and type key = K.t = struct


    (** Tag *)
    module Tag: sig
      type t
      type gen
      val mk_gen: unit -> gen
      val to_int: t -> int
      val dtag : t
      val next_tag: gen -> t
      val incr_tag: gen -> unit
      (** all of them are different from dtag *)
    end = struct
      type t = int
      type gen = int ref
      let to_int x = x
      let dtag = min_int (** tag used in the polymorphic non hashconsed case *)
      let mk_gen () = ref (min_int + 1)
      let next_tag gen = !gen
      let incr_tag gen = incr gen; assert (!gen != dtag)

    end

    module M : sig
      type (+'a) t
      type 'a data = Data.t
      val nt: 'a data t -> 'a data NT.t
      val rebuild: 'a data NT.t -> 'a data t
      type 'a view = private
        | Empty
        | Lf of K.t * 'a
        | Br of int * 'a t * 'a t
      val view: 'a data t -> 'a data view
      val tag : 'a data t -> int
      val ktag : 'a data t -> int
      val mk_Empty: 'a data t
      val mk_Lf: K.t -> 'a data -> 'a data t
      val mk_Br: int -> 'a data t -> 'a data t -> 'a data t
    end = struct
      module Check = struct
        type 'a def = 'a Def.t =  (** check the type of Def.t *)
          | Empty
          | Lf of K.t * 'a
          | Br of int * 'a def * 'a def
      end

      type 'a t =
        | Empty
        | Lf of K.t * 'a * Tag.t
        | Br of int * 'a t * 'a t * Tag.t

      type 'a data = Data.t

      (** This obj.magic "just" hide the last field *)
      let nt x = (Obj.magic (x : 'a t) : 'a Check.def)

      let tag = function
        | Empty -> Tag.to_int Tag.dtag
        | Lf(_,_,tag) | Br(_,_,_,tag) -> Tag.to_int tag

      let ktag = function
        | Empty -> assert false (** absurd: Should'nt be used on Empty *)
        | Lf(k,_,_) -> K.tag k
        | Br(i,_,_,_) -> i

      module WH = Weak.Make(struct
          type 'a t' = 'a t
          type t = Data.t t'

          let equal x y =
            match x, y with
            | Empty, Empty -> true
            | Lf(i1,d1,_), Lf(i2,d2,_) ->
              K.equal i1 i2 && Data.equal d1 d2
            | Br(_,l1,r1,_), Br(_,l2,r2,_) -> l1 == l2 && r1 == r2
            | _ -> false

          let hash = function
            | Empty -> 0
            | Lf(i1,d1,_) ->
              65599 * ((K.tag i1) + (Data.hash d1 * 65599 + 31))
            | Br(_,l,r,_) ->
              65599 * ((tag l) + ((tag r) * 65599 + 17))
        end)

      let gentag = Tag.mk_gen ()
      let htable = WH.create 5003

      let hashcons d =
        let o = WH.merge htable d in
        if o == d then Tag.incr_tag gentag;
        o

      let mk_Empty = Empty
      let mk_Lf k x = hashcons (Lf(k,x,Tag.next_tag gentag))
      let mk_Br k t0 t1 =
        (* assert (t0 != Empty && t1 != Empty); *)
        hashcons (Br(k,t0,t1,Tag.next_tag gentag))


      let rec rebuild t = match t with
        | Def.Empty -> mk_Empty
        | Def.Lf(i,d) -> mk_Lf i d
        | Def.Br(i,l,r) -> mk_Br i (rebuild l) (rebuild r)

      type 'a view =
        | Empty
        | Lf of K.t * 'a
        | Br of int * 'a t * 'a t

      (** This obj.magic "just" hide the last field of the root node *)
      let view x = (Obj.magic (x : 'a t): 'a view)

    end

    include Gen(M)

    let mk_Empty = M.mk_Empty
    let mk_Lf = M.mk_Lf

    let nt = M.nt
    let rebuild = M.rebuild

    let compare_t s t = Pervasives.compare (M.tag s) (M.tag t)
    let equal_t (s:'a data t) t = s == t

    (** with Def.t *)
    let rec interi_nt lf_phi s t =
      match M.view s , t with
      | M.Empty , _ -> mk_Empty
      | _ , Def.Empty -> mk_Empty
      | M.Lf(i,x) , Def.Lf(j,y) ->
        if K.equal i j
        then lf_phi i x y
        else mk_Empty
      | M.Lf(i,x) , Def.Br _ ->
        (match NT.occur i t with None -> mk_Empty | Some y -> lf_phi i x y)
      | M.Br _ , Def.Lf(j,y) ->
        (match occur j s with None -> mk_Empty | Some x -> lf_phi j x y)
      | M.Br(p,s0,s1) , Def.Br(q,t0,t1) ->
        if p == q then
          (* prefixes agree *)
          glue (interi_nt lf_phi s0 t0) (interi_nt lf_phi s1 t1)
        else if included_prefix p q then
          (* q contains p. Intersect t with a subtree of s *)
          if zero_bit q p
          then interi_nt lf_phi s0 t (* t has bit m = 0 => t is inside s0 *)
          else interi_nt lf_phi s1 t (* t has bit m = 1 => t is inside s1 *)
        else if included_prefix q p then
          (* p contains q. Intersect s with a subtree of t *)
          if zero_bit p q
          then interi_nt lf_phi s t0 (* s has bit n = 0 => s is inside t0 *)
          else interi_nt lf_phi s t1 (* t has bit n = 1 => s is inside t1 *)
        else
          (* prefix disagree *)
          mk_Empty

    let inter_nt phi = interi_nt (fun i x y -> mk_Lf i (phi i x y))
    let interf_nt phi = interi_nt (fun i x y -> lf i (phi i x y))
    let set_inter_nt m1 m2 = interi_nt (fun i x _ -> mk_Lf i x) m1 m2

  end

end
