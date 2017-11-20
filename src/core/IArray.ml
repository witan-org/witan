(*************************************************************************)
(*  This file is part of Witan.                                          *)
(*                                                                       *)
(*  Copyright (C) 2017                                                   *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies           *)
(*           alternatives)                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en        *)
(*           Automatique)                                                *)
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

type 'a t = 'a array

let of_list = Array.of_list
let of_array = Array.copy
let of_iter l (iter : ('a -> unit) -> unit) =
  if l = 0 then [||] else
    let res = Array.make l (Obj.magic 0 : 'a) in
    let r = ref 0 in
    iter (fun v -> res.(!r) <- v; incr r);
    assert (!r == l);
    res



let length = Array.length

let equal cmp t1 t2 =
  Array.length t1 = Array.length t2 &&
  let last = Array.length t1 - 1 in
  try
    for i = 0 to last do
      if not (cmp t1.(i) t2.(i)) then raise Exit
    done;
    true
  with Exit -> false

let compare cmp t1 t2 =
  let lt1 = Array.length t1 in
  let c = compare lt1 (Array.length t2) in
  if c <> 0 then c else
    match lt1 with
    | 0 -> 0
    | 1  ->
      cmp t1.(0) t2.(0)
    | 2  ->
      let c = cmp t1.(0) t2.(0) in
      if c <> 0 then c else
      cmp t1.(1) t2.(1)
    | 3  ->
      let c = cmp t1.(0) t2.(0) in
      if c <> 0 then c else
      let c = cmp t1.(1) t2.(1) in
      if c <> 0 then c else
      cmp t1.(2) t2.(2)
    | 4  ->
      let c = cmp t1.(0) t2.(0) in
      if c <> 0 then c else
      let c = cmp t1.(1) t2.(1) in
      if c <> 0 then c else
      let c = cmp t1.(2) t2.(2) in
      if c <> 0 then c else
      cmp t1.(3) t2.(3)
    | _ ->
      let rec aux t1 t2 = function
        | -1 -> 0
        | i  -> let c = cmp t1.(i) t2.(i) in
                if c <> 0  then c else aux t1 t2 (i-1)
      in
      aux t1 t2 (lt1-1)

let hash h t =
  let last = Array.length t - 1 in
  let c = ref last in
  for i = 0 to last do
    c := Hashcons.combine (h t.(i)) !c
  done;
  !c

let get = Array.get

let iter = Array.iter
let iteri = Array.iteri
let fold = Array.fold_left

let foldi f x a =
  let r = ref x in
  for i = 0 to Array.length a - 1 do
    r := f i !r (Array.unsafe_get a i)
  done;
  !r

let pp sep p fmt a = Pp.iter1 iter sep p fmt a
