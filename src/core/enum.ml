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

type 'a t =
  | List: (** content *) 'b list *
       (** filter  *) ('b -> bool) *
       (** map *)     ('b -> 'a)     -> 'a t
  | Bag: (** content *) 'b Bag.t *
       (** filter  *) ('b -> bool) *
       (** map *)     ('b -> 'a)     -> 'a t

let fold f acc t =
  match t with
  | List (content,filter,map) ->
    List.fold_left
      (fun acc x -> if filter x then f acc (map x) else acc)
      acc content
  | Bag (content,filter,map) ->
    Bag.fold_left
      (fun acc x -> if filter x then f acc (map x) else acc)
      acc content

let iter f t =
  match t with
  | List (content,filter,map) ->
    List.iter (fun x -> if filter x then f (map x)) content
  | Bag (content,filter,map) ->
    Bag.iter (fun x -> if filter x then f (map x)) content

let for_all f t =
  match t with
  | List (content,filter,map) ->
    List.for_all (fun x -> not (filter x) || f (map x)) content
  | Bag (content,filter,map) ->
    Bag.for_all (fun x -> not (filter x) || f (map x)) content

let exists f t =
  match t with
  | List (content,filter,map) ->
    List.exists (fun x -> filter x && f (map x)) content
  | Bag (content,filter,map) ->
    Bag.exists (fun x -> filter x && f (map x)) content

let is_empty t = exists (fun _ -> true) t

let list_rev t =
  match t with
  | List (content,filter,map) ->
    List.fold_left
      (fun acc x -> if filter x then (map x)::acc else acc)
      [] content
  | Bag (content,filter,map) ->
    Bag.fold_left
      (fun acc x -> if filter x then (map x)::acc else acc)
      [] content

let from_list ?(filter=(fun _ -> true)) ~map content =
  List(content,filter,map)

let from_bag ?(filter=(fun _ -> true)) ~map content =
  Bag(content,filter,map)
