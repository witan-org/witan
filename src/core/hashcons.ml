(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2017   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

open Stdlib
(*s Hash tables for hash-consing. (Some code is borrowed from the ocaml
    standard library, which is copyright 1996 INRIA.) *)

module type HashedType =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
    val set_tag : int -> t -> t
    val tag : t -> int
    val pp: t Pp.pp
  end

module type S =
  sig
    include Datatype
    val hashcons : t -> t
    val hashcons0: (int -> t) -> t
    val hashcons1: (int -> 'a -> t) -> 'a -> t
    val hashcons2: (int -> 'a -> 'b -> t) -> 'a -> 'b -> t
    val hashcons3: (int -> 'a -> 'b -> 'c -> t) -> 'a -> 'b -> 'c -> t
    val iter : (t -> unit) -> unit
    type cat
    val stats : cat -> int * int * int * int * int * int
    val fresh_tag: unit -> int
  end


module MakeTag(H : sig include HashedType
                 val next_tag: unit -> int
                 val incr_tag: unit -> unit
               end) :
  (S with type t = H.t and type cat := unit) =
struct
  module WH = Weak.Make (H)

  let fresh_tag () = let tag = H.next_tag () in H.incr_tag (); tag

  let htable = WH.create 5003

  let hashcons' d =
    let o = WH.merge htable d in
    if o == d then H.incr_tag ();
    o

  let hashcons d = hashcons' (H.set_tag (H.next_tag ()) d)
  let hashcons0 f = hashcons' (f (H.next_tag ()))
  let hashcons1 f a = hashcons' (f (H.next_tag ()) a)
  let hashcons2 f a b = hashcons' (f (H.next_tag ()) a b)
  let hashcons3 f a b c = hashcons' (f (H.next_tag ()) a b c)

  let iter f = WH.iter f htable

  let stats () = WH.stats htable

  module T = struct
    type t = H.t
    let hash = H.tag
    let equal ts1 ts2 = ts1 == ts2
    let compare ts1 ts2 = Pervasives.compare (H.tag ts1) (H.tag ts2)
    let pp = H.pp
  end
  include T
  include MkDatatype(T)

end

module Make(H : HashedType) :
  (S with type t = H.t and type cat := unit) =
  MakeTag(struct
    include H
    let next_tag, incr_tag = Util.get_counter ()
  end)

module MakeCat(H : sig include HashedType val category: t -> int end) :
  (sig include S with type t = H.t and type cat := int
    val new_category: int -> unit
    val iter_cat: (t -> unit) -> int -> unit
  end) =
struct
  module WH = Weak.Make (H)

  let next_tag = ref 0
  let fresh_tag () = let tag = !next_tag in incr next_tag; tag

  let htable: WH.t Simple_vector.t = Simple_vector.create 10

  let new_category cat =
    Simple_vector.inc_size cat htable;
    assert (Simple_vector.is_uninitialized htable cat);
    Simple_vector.set htable cat (WH.create 64)

  let hashcons' d =
    let o = WH.merge (Simple_vector.get htable (H.category d)) d in
    if o == d then incr next_tag;
    o

  let hashcons d = hashcons' (H.set_tag !next_tag d)
  let hashcons0 f = hashcons' (f !next_tag)
  let hashcons1 f a = hashcons' (f !next_tag a)
  let hashcons2 f a b = hashcons' (f !next_tag a b)
  let hashcons3 f a b c = hashcons' (f !next_tag a b c)

  let iter f =
    Simple_vector.iter_initialized (fun h -> WH.iter f h) htable

  let iter_cat f cat =
    WH.iter f (Simple_vector.get htable cat)

  let stats cat = WH.stats (Simple_vector.get htable cat)

  module T = struct
    type t = H.t
    let hash = H.tag
    let equal ts1 ts2 = ts1 == ts2
    let compare ts1 ts2 = Pervasives.compare (H.tag ts1) (H.tag ts2)
    let pp = H.pp
  end
  include T
  include MkDatatype(T)

end


let combine acc n = n * 65599 + acc
let combine2 acc n1 n2 = combine acc (combine n1 n2)
let combine3 acc n1 n2 n3 = combine acc (combine n1 (combine n2 n3))
let combine_list f = List.fold_left (fun acc x -> combine acc (f x))
let combine_option h = function None -> 0 | Some s -> (h s) + 1
let combine_pair h1 h2 (a1,a2) = combine (h1 a1) (h2 a2)

