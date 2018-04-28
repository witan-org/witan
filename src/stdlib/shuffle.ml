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

(** {!shuffle} is used for test. Used for shuffling input entry *)
let opt_shuffle = ref None

let make_random =
  let h = Hashtbl.create 10 in
  fun i ->
    try
      Random.State.copy (Hashtbl.find h i)
    with Not_found ->
      let s = Random.State.make i in
      Hashtbl.add h i s;
      Random.State.copy s

let int m = 
  match !opt_shuffle with
  | Some rnd -> Random.State.int rnd m
  | None -> max 0 (m-1)

let set_shuffle = function
  | None -> opt_shuffle := None
  | Some i -> opt_shuffle := Some (make_random i)

let is_shuffle () = match !opt_shuffle with | None -> false | Some _ -> true

let shuffle2 ((t1,t2) as p) =
  match !opt_shuffle with
  | None -> p
  | Some rnd when Random.State.bool rnd -> p
  | _ -> (t2,t1)

let seq2 f (t1,t2) =
  match !opt_shuffle with
  | Some rnd when Random.State.bool rnd ->
    let t2 = f t2 in
    let t1 = f t1 in
    (t1,t2)
  | Some _ | None ->
    let t1 = f t1 in
    let t2 = f t2 in
    (t1,t2)


let shuffle3 ((t1,t2,t3) as p) =
  match !opt_shuffle with
  | None -> p
  | Some rnd ->
    match Random.State.int rnd 6 with
    | 0 -> p
    | 1 -> (t1,t3,t2)
    | 2 -> (t2,t1,t3)
    | 3 -> (t2,t3,t2)
    | 4 -> (t3,t1,t2)
    | 5 -> (t3,t2,t1)
    | _ -> assert false

let seq3 f (t1,t2,t3) =
  match !opt_shuffle with
  | None -> let t1 = f t1 in
            let t2 = f t2 in
            let t3 = f t3 in
            (t1,t2,t3)
  | Some rnd ->
    match Random.State.int rnd 6 with
    | 0 -> let t1 = f t1 in
           let t2 = f t2 in
           let t3 = f t3 in
           (t1,t2,t3)
    | 1 -> let t1 = f t1 in
           let t3 = f t3 in
           let t2 = f t2 in
           (t1,t2,t3)
    | 2 -> let t2 = f t2 in
           let t1 = f t1 in
           let t3 = f t3 in
           (t1,t2,t3)
    | 3 -> let t2 = f t2 in
           let t3 = f t3 in
           let t1 = f t1 in
           (t1,t2,t3)
    | 4 -> let t3 = f t3 in
           let t1 = f t1 in
           let t2 = f t2 in
           (t1,t2,t3)
    | 5 -> let t3 = f t3 in
           let t2 = f t2 in
           let t1 = f t1 in
           (t1,t2,t3)
    | _ -> assert false


let shufflel l =
  match !opt_shuffle with
  | None -> l
  | Some rnd ->
    let rec aux head tail = function
      | [] -> List.rev_append head tail
      | a::l when Random.State.bool rnd -> aux (a::head) tail l
      | a::l                            -> aux head (a::tail) l in
    aux [] [] l

let seql' f l =
  match !opt_shuffle with
  | None -> List.iter f l
  | Some rnd ->
    let rec aux head tail = function
      | [] -> List.iter f head; List.iter f tail
      | a::l when Random.State.bool rnd -> aux (a::head) tail l
      | a::l                            -> aux head (a::tail) l in
    aux [] [] l

let app = (fun f -> f ())
let seql l =
  match !opt_shuffle with
  | None -> List.iter app l
  | Some rnd ->
    let rec aux head tail = function
      | [] -> List.iter app head; List.iter app tail
      | a::l when Random.State.bool rnd -> aux (a::head) tail l
      | a::l                            -> aux head (a::tail) l in
    aux [] [] l


let chooseb f g t =
  match !opt_shuffle with
  | None -> f t
  | Some rnd -> g (fun () -> Random.State.bool rnd) t

let choosef f g t =
  match !opt_shuffle with
  | None -> f t
  | Some rnd -> g (Random.State.float rnd) t

let choosei f g t =
  match !opt_shuffle with
  | None -> f t
  | Some rnd -> g (Random.State.int rnd) t
