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

type 'a t = {mutable data: 'a array;
             mutable size: int}

let dumb : 'a. 'a =
  let dumb_addr = ref 0 in
  Obj.magic dumb_addr

let create size = { data = Array.make size dumb; size = size }

let size t = t.size

let get_dumb t i =
  assert (i < t.size);
  let r = t.data.(i) in
  r

let get t i =
  assert (i < t.size);
  let r = t.data.(i) in
  assert (r != dumb);
  r

let get_def t i def =
  assert (i < t.size);
  let r = t.data.(i) in
  if r == dumb then def else r

let set t i v =
  assert (i < t.size);
  t.data.(i) <- v

let is_uninitialized t i = t.data.(i) == dumb
let uninitialize     t i = t.data.(i) <- dumb

let clear t = Array.fill t.data 0 t.size dumb

let init_inc_size new_size f t =
  if t.size < new_size then
    let old_capacity = Array.length t.data in
    if old_capacity < new_size then
      let init i =
        if i < t.size then t.data.(i)
        else if i < new_size then f i
        else dumb in
      let new_capacity = max (old_capacity * 2) new_size in
      let new_data = Array.init new_capacity init in
      t.size <- new_size;
      t.data <- new_data
    else begin
      for i = t.size to new_size - 1 do
        t.data.(i) <- f i
      done;
      t.size <- new_size
    end

let inc_size new_size t =
  if t.size < new_size then
    let old_capacity = Array.length t.data in
    if old_capacity < new_size then
      let new_capacity = max (old_capacity * 2) new_size in
      let new_data = Array.make new_capacity dumb in
      Array.blit t.data 0 new_data 0 t.size;
      t.size <- new_size;
      t.data <- new_data
    else
      t.size <- new_size

let push t v =
  let i = t.size in
  inc_size (i + 1) t;
  set t i v

let drop_last t =
  assert (0 < t.size);
  t.size <- t.size - 1;
  uninitialize t t.size

let iter_initialized f t =
  for i = 0 to t.size - 1 do
    let e = t.data.(i) in
    if e != dumb then f e
  done

let fold_initialized f acc t =
  let r = ref acc in
  for i = 0 to t.size - 1 do
    let e = t.data.(i) in
    if e != dumb then r := f !r e
  done;
  !r

let apply_initialized f t =
  for i = 0 to t.size - 1 do
    let e = t.data.(i) in
    if e != dumb then t.data.(i) <- f e
  done

let iter_initializedi f t =
  for i = 0 to t.size - 1 do
    let e = t.data.(i) in
    if e != dumb then f i e
  done

let fold_initializedi f acc t =
  let r = ref acc in
  for i = 0 to t.size - 1 do
    let e = t.data.(i) in
    if e != dumb then r := f !r i e
  done;
  !r


let copy t = { data = Array.copy t.data; size = t.size}
