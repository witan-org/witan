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

open Std
open Witan_popop_lib
    
include Map_hetero_sig
    
module MakeS
  (K: Keys)
  (D:sig type ('a,'b) t end)
  = struct
    
  type 'a key       = 'a K.t
  type ('a,'b) data = ('a,'b) D.t
  type 'b pair      = Pair : 'a K.t * ('a,'b) D.t -> 'b pair

  module GIM = Intmap.Make(struct include Int let tag x = x end)
  module IM  = GIM.NT
  type 'b t  = 'b pair IM.t

  let eq (type a a') (k : a key) (k' : a' key) : (a,a') Poly.eq =
    match K.equal k k' with
    | Poly.Eq  -> Poly.Eq
    | Poly.Neq -> raise IncoherentMap

    
  let empty      = IM.empty
  let is_empty   = IM.is_empty
  let set_submap = IM.set_submap
  
  let singleton (type a) (type b) (k: a K.t) (d : (a,b) data) : b t =
    IM.singleton (K.tag k) (Pair(k,d))

  let add (type a) (type b) (k : a K.t) d (t : b t) =
    IM.add (K.tag k) (Pair(k,d)) t

  let change (type a) (type b) f (k : a K.t) (t : b t) =
    let f = function
      | None -> f None
      | Some(Pair(k',v)) -> let Poly.Eq = eq k k' in f(Some (v : (a,b) data))
    in
    IM.change (fun x -> f x |> Option.map (fun v -> Pair(k,v))) (K.tag k) t

  let add_change (type a) (type b) empty add (k : a K.t) v (t : b t) =
    let empty x = Pair(k,empty x) in
    let add x (Pair(k',v')) = let Poly.Eq = eq k k' in Pair(k, add x (v':(a,b)data)) in
    IM.add_change empty add (K.tag k) v t

  let find_common (type a) (k : a K.t) (Pair(k',v) : 'b pair) : (a,'b) data =
    let Poly.Eq = eq k k' in v

  let find (type a) (k : a K.t) (t : 'b t) : (a,'b) data =
    IM.find (K.tag k) t |> find_common k

  let find_opt (type a) (k : a K.t) t =
    IM.find_opt (K.tag k) t |> Option.map (find_common k)

  let find_def (type a) def (k : a K.t) t =
    find_opt k t |> Option.get_or ~default:def

  let find_exn (type a) exn (k : a K.t) t =
    IM.find_exn exn (K.tag k) t |> find_common k


  type 'b union =
    { union: 'a. 'a key -> ('a,'b) data -> ('a,'b) data -> ('a,'b) data option }

  let union f t1 t2 =
    IM.union
      (fun _ (Pair(k1,d1)) (Pair(k2,d2)) ->
         let Poly.Eq = eq k1 k2 in Option.map (fun d -> Pair(k1,d)) (f.union k1 d1 d2))
      t1 t2

  type ('b,'c) fold2_inter =
    { fold2_inter: 'a. 'a key -> ('a,'b) data -> ('a,'b) data -> 'c -> 'c }

  let fold2_inter f t1 t2 acc =
    IM.fold2_inter
      (fun _ (Pair(k1,d1)) (Pair(k2,d2)) acc ->
         let Poly.Eq = eq k1 k2 in f.fold2_inter k1 d1 d2 acc)
      t1 t2 acc

  type 'b iter = { iter: 'a. 'a key -> ('a,'b) data -> unit }

  let iter f t = IM.iter (fun _ (Pair(k,d)) -> f.iter k d) t

  type ('b,'c) fold = { fold: 'a. 'c -> 'a key -> ('a,'b) data -> 'c }

  let fold f acc t = IM.fold_left (fun acc _ (Pair(k,d)) -> f.fold acc k d) acc t

  type 'b mapi = { mapi: 'a. 'a key -> ('a,'b) data -> ('a,'b) data }
  let mapi f t = IM.mapi (fun _ (Pair(k,d)) -> Pair(k,f.mapi k d)) t

  type printk = { printk : 'a. 'a key Format.printer }
  type 'b printd = { printd : 'a. 'a key -> ('a, 'b) data Format.printer }

  let pp (sep1 : unit Format.printer) (sep2 : unit Format.printer)
      (printkey : printk) (printdata : 'b printd) : 'b t Format.printer
    =
    fun fmt t ->
      let printkeydata fmt (Pair(k,v)) =
        Format.fprintf fmt "%a%a%a" printkey.printk k sep2 () (printdata.printd k) v
      in
      let as_list = IM.fold (fun _ v sofar -> v::sofar) t [] in
      Format.list ~sep:sep1 printkeydata fmt as_list

end


module MakeR (K:Keys) = struct
  include MakeS(K)(struct type ('a,'b) t = 'b end)

  let pp (type b) (sep1 : unit Format.printer) (sep2 : unit Format.printer)
      (printkey : printk) (printdata : b Format.printer) : b t Format.printer
    =
    fun fmt t ->
      let printkeydata fmt (Pair(k,v)) =
        Format.fprintf fmt "%a%a%a" printkey.printk k sep2 () printdata v
      in
      let as_list = IM.fold (fun _ v sofar -> v::sofar) t [] in
      Format.list ~sep:sep1 printkeydata fmt as_list

end
