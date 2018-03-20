(***********************************************************************)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*    en Automatique.                                                  *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Lesser General Public License version 2.1, with the        *)
(*  special exception on linking described in the file LICENSE.        *)
(***********************************************************************)

(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2012   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

(* To pick random seeds if requested *)

let randomized_default =
  let params =
    try Sys.getenv "OCAMLRUNPARAM" with Not_found ->
    try Sys.getenv "CAMLRUNPARAM" with Not_found -> "" in
  String.contains params 'R'

let randomized = ref randomized_default

let randomize () = randomized := true

let prng = lazy (Random.State.make_self_init())


(** {6 Functorial interface} *)


module type HashedType =
  sig
    type t
      (** The type of the hashtable keys. *)
    val equal : t -> t -> bool
      (** The equality predicate used to compare keys. *)
    val hash : t -> int
      (** A hashing function on keys. It must be such that if two keys are
          equal according to [equal], then they have identical hash values
          as computed by [hash].
          Examples: suitable ([equal], [hash]) pairs for arbitrary key
          types include
-         ([(=)], {!Hashtbl.hash}) for comparing objects by structure
              (provided objects do not contain floats)
-         ([(fun x y -> compare x y = 0)], {!Hashtbl.hash})
              for comparing objects by structure
              and handling {!Pervasives.nan} correctly
-         ([(==)], {!Hashtbl.hash}) for comparing objects by physical
              equality (e.g. for mutable or cyclic objects). *)
   end
(** The input signature of the functor {!Hashtbl.Make}. *)

type statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array
}

module type S =
  sig
    type key
    type 'a t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats: 'a t -> statistics
  val find_def : 'a t -> 'a -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_exn : 'a t -> exn -> key -> 'a
  val mapi : (key -> 'a -> 'a) -> 'a t -> unit
  val memo : (key -> 'a) -> 'a t -> key -> 'a
  val is_empty : 'a t -> bool
  val remove_all: 'a t -> key -> unit
  val change   : ('a option -> 'a option) -> 'a t -> key -> unit
  val add_new  : exn -> 'a t -> key -> 'a -> unit

  end
(** The output signature of the functor {!Hashtbl.Make}. *)

module type Private =
sig
  type 'a t
  type key
  val find : 'a t -> key -> 'a
  val find_def : 'a t -> 'a -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_exn : 'a t -> exn -> key -> 'a
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val mem : 'a t -> key -> bool
  val length : 'a t -> int
  val is_empty : 'a t -> bool
end

module type Hashtbl =
sig
val hash : 'a -> int
module type HashedType = HashedType
module type S = S
module type Private = Private
module Make (X:Hashtbl.HashedType) : S with type key = X.t
end

module Hashtbl =
struct

let hash = Hashtbl.hash

module type HashedType = HashedType
module type S = S
module type Private = Private


module type SeededHashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: int -> t -> int
  end

module MakeSeeded(H: SeededHashedType) =
  struct
    type key = H.t
    (* We do dynamic hashing, and resize the table and rehash the elements
       when buckets become too long. *)

    type 'b t =
      { mutable size: int;                        (* number of entries *)
        mutable data: 'b bucketlist array;  (* the buckets *)
        mutable seed: int;                        (* for randomization *)
        initial_size: int;                        (* initial array size *)
      }

    and 'b bucketlist =
        Empty
      | Cons of key * 'b * 'b bucketlist


    let rec power_2_above x n =
      if x >= n then x
      else if x * 2 > Sys.max_array_length then x
      else power_2_above (x * 2) n

    let create ?(random = !randomized) initial_size =
      let s = power_2_above 16 initial_size in
      let seed = if random then Random.State.bits (Lazy.force prng) else 0 in
      { initial_size = s; size = 0; seed = seed; data = Array.make s Empty }

    let clear h =
      h.size <- 0;
      let len = Array.length h.data in
      for i = 0 to len - 1 do
        h.data.(i) <- Empty
      done

    let reset h =
      let len = Array.length h.data in
      if Obj.size (Obj.repr h) < 4 (* compatibility with old hash tables *)
      || len = h.initial_size then
        clear h
      else begin
        h.size <- 0;
        h.data <- Array.make h.initial_size Empty
      end

    let copy h = { h with data = Array.copy h.data }

    let key_index h key =
      (H.hash h.seed key) land (Array.length h.data - 1)


    let resize indexfun h =
      let odata = h.data in
      let osize = Array.length odata in
      let nsize = osize * 2 in
      if nsize < Sys.max_array_length then begin
        let ndata = Array.make nsize Empty in
        h.data <- ndata;        (* so that indexfun sees the new bucket count *)
        let rec insert_bucket = function
            Empty -> ()
          | Cons(key, data, rest) ->
            insert_bucket rest; (* preserve original order of elements *)
            let nidx = indexfun h key in
            ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
        for i = 0 to osize - 1 do
          insert_bucket odata.(i)
        done
      end

    let add h key info =
      let i = key_index h key in
      let bucket = Cons(key, info, h.data.(i)) in
      h.data.(i) <- bucket;
      h.size <- h.size + 1;
      if h.size > Array.length h.data lsl 1 then resize key_index h

    let remove h key =
      let rec remove_bucket = function
        | Empty ->
            Empty
        | Cons(k, i, next) ->
            if H.equal k key
            then begin h.size <- h.size - 1; next end
            else Cons(k, i, remove_bucket next) in
      let i = key_index h key in
      h.data.(i) <- remove_bucket h.data.(i)

    let rec find_rec exn key = function
      | Empty ->
          raise exn
      | Cons(k, d, rest) ->
          if H.equal key k then d else find_rec exn key rest

    let find_exn h exn key =
      match h.data.(key_index h key) with
      | Empty -> raise exn
      | Cons(k1, d1, rest1) ->
          if H.equal key k1 then d1 else
          match rest1 with
          | Empty -> raise exn
          | Cons(k2, d2, rest2) ->
              if H.equal key k2 then d2 else
              match rest2 with
              | Empty -> raise exn
              | Cons(k3, d3, rest3) ->
                  if H.equal key k3 then d3 else find_rec exn key rest3

    let find_all h key =
      let rec find_in_bucket = function
      | Empty ->
          []
      | Cons(k, d, rest) ->
          if H.equal k key
          then d :: find_in_bucket rest
          else find_in_bucket rest in
      find_in_bucket h.data.(key_index h key)

    let replace h key info =
      let rec replace_bucket = function
        | Empty ->
            raise Not_found
        | Cons(k, i, next) ->
            if H.equal k key
            then
              if i == info then raise Exit
              else Cons(key, info, next)
            else Cons(k, i, replace_bucket next) in
      let i = key_index h key in
      let l = h.data.(i) in
      try
        h.data.(i) <- replace_bucket l
      with
      | Not_found ->
        h.data.(i) <- Cons(key, info, l);
        h.size <- h.size + 1;
        if h.size > Array.length h.data lsl 1 then resize key_index h
      | Exit -> (**same value than before: addition from ocaml version *) ()


    (** addition from ocaml version *)
    let change f h key =
      let rec replace_bucket = function
        | Empty ->
            raise Not_found
        | Cons(k, i, next) ->
            if H.equal k key
            then
              match f (Some i) with
              | None -> next
              | Some info ->
                if i == info then raise Exit
                else Cons(key, info, next)
            else Cons(k, i, replace_bucket next) in
      let i = key_index h key in
      let l = h.data.(i) in
      try
        h.data.(i) <- replace_bucket l
      with
      | Exit -> ()
      | Not_found ->
        match f None with
        | None -> ()
        | Some info ->
          h.data.(i) <- Cons(key, info, l);
          h.size <- h.size + 1;
          if h.size > Array.length h.data lsl 1 then resize key_index h

    let add_new e h x v = change (function
      | Some _ -> raise e
      | None -> Some v) h x

    let mem h key =
      let rec mem_in_bucket = function
      | Empty ->
          false
      | Cons(k, _, rest) ->
          H.equal k key || mem_in_bucket rest in
      mem_in_bucket h.data.(key_index h key)

    let iter f h =
      let rec do_bucket = function
        | Empty ->
          ()
        | Cons(k, d, rest) ->
          f k d; do_bucket rest in
      let d = h.data in
      for i = 0 to Array.length d - 1 do
        do_bucket d.(i)
      done

    let fold f h init =
      let rec do_bucket b accu =
        match b with
          Empty ->
          accu
        | Cons(k, d, rest) ->
          do_bucket rest (f k d accu) in
      let d = h.data in
      let accu = ref init in
      for i = 0 to Array.length d - 1 do
        accu := do_bucket d.(i) !accu
      done;
      !accu


    let mapi f h =
      let rec do_bucket f b =
        match b with
          Empty -> Empty
        | Cons(k, d, rest) ->
          Cons(k, f k d, do_bucket f rest) in
      let d = h.data in
      for i = 0 to Array.length d - 1 do
        d.(i) <- do_bucket f d.(i)
      done

    let length h = h.size

    let rec bucket_length accu = function
      | Empty -> accu
      | Cons(_, _, rest) -> bucket_length (accu + 1) rest

    let stats h =
      let mbl =
        Array.fold_left (fun m b -> max m (bucket_length 0 b)) 0 h.data in
      let histo = Array.make (mbl + 1) 0 in
      Array.iter
        (fun b ->
           let l = bucket_length 0 b in
           histo.(l) <- histo.(l) + 1)
        h.data;
      { num_bindings = h.size;
        num_buckets = Array.length h.data;
        max_bucket_length = mbl;
        bucket_histogram = histo }

    let find h k = find_exn h Not_found k

    let memo f h x =
      try find h x
      with Not_found -> let y = f x in add h x y; y

    let find_def h d k =
      try find h k with Not_found -> d

    let find_opt h k =
      try Some (find h k) with Not_found -> None

    let is_empty h = length h = 0

    let rec remove_all h e =
      if mem h e then
        begin remove h e; remove_all h e end



  end

module Make(H: HashedType): (S with type key = H.t) =
  struct
    include MakeSeeded(struct
        type t = H.t
        let equal = H.equal
        let hash (_seed: int) x = H.hash x
      end)
    let create sz = create ~random:false sz
  end
end
