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

(* This file originates from the OCaml v 3.12 Standard Library.
   It was extended and modified for the needs of the Why3 project.
   It is distributed under the terms of its initial license, which
   is provided in the file OCAML-LICENSE. *)

(** Input signature of the functor {!Extmap.Make} and {!Extset.Make}. *)
module type OrderedType = sig
  type t
  val compare: t -> t -> int
  val pp: t Pp.pp
end

(** Input signature of the functor {!Intmap.Make}. *)
module type TaggedEqualType =
sig
  type t
  val tag : t -> int
  val equal : t -> t -> bool
  val pp: t Pp.pp
end

(** Input signature of the functor {!Intmap.Make.Make}. *)
module type HashType =
sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
  val pp: t Pp.pp
end

(** Output signature of the functor {!Extmap.Make}. *)
module type Map =
  sig
    type key
    (** The type of the map keys. *)

    type 'a data
    (** The type of the data (* used for genericity of the interface *) *)

    type (+'a) t
    (** The type of maps from type [key] to type ['a]. *)

    val empty: 'a data t
    (** The empty map. *)

    val is_empty: 'a data t -> bool
    (** Test whether a map is empty or not. *)

    val mem: key -> 'a data t -> bool
    (** [mem x m] returns [true] if [m] contains a binding for [x],
        and [false] otherwise. *)

    val add: key -> 'a data -> 'a data t -> 'a data t
    (** [add x y m] returns a map containing the same bindings as
        [m], plus a binding of [x] to [y]. If [x] was already bound
        in [m], its previous binding disappears. *)

    val singleton: key -> 'a data -> 'a data t
    (** [singleton x y] returns the one-element map that contains a binding [y]
        for [x]. *)

    val remove: key -> 'a data t -> 'a data t
    (** [remove x m] returns a map containing the same bindings as
        [m], except for [x] which is unbound in the returned map. *)

    val merge:
         (key -> 'a data option -> 'b data option -> 'c data option) ->
         'a data t -> 'b data t -> 'c data t
    (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
        and of [m2]. The presence of each such binding, and the corresponding
        value, is determined with the function [f]. *)

    val compare: ('a data -> 'a data -> int) -> 'a data t -> 'a data t -> int
    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. *)

    val equal: ('a data -> 'a data -> bool) -> 'a data t -> 'a data t -> bool
    (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
        equal, that is, contain equal keys and associate them with
        equal data.  [cmp] is the equality predicate used to compare
        the data associated with the keys. *)

    val pp: 'a data Pp.pp -> 'a data t Pp.pp

    val iter: (key -> 'a data -> unit) -> 'a data t -> unit
    (** [iter f m] applies [f] to all bindings in map [m].
       [f] receives the key as first argument, and the associated value
       as second argument.  The bindings are passed to [f] in increasing
       order with respect to the ordering over the type of the keys. *)

    val fold: (key -> 'a data -> 'b -> 'b) -> 'a data t -> 'b -> 'b
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)], where
        [k1 ... kN] are the keys of all bindings in [m] (in increasing
        order), and [d1 ... dN] are the associated data. *)

    val for_all: (key -> 'a data -> bool) -> 'a data t -> bool
    (** [for_all p m] checks if all the bindings of the map
        satisfy the predicate [p]. *)

    val exists: (key -> 'a data -> bool) -> 'a data t -> bool
    (** [exists p m] checks if at least one binding of the map
        satisfy the predicate [p]. *)

    val filter: (key -> 'a data -> bool) -> 'a data t -> 'a data t
    (** [filter p m] returns the map with all the bindings in [m]
        that satisfy predicate [p]. *)

    val partition:
      (key -> 'a data -> bool) ->
      'a data t -> 'a data t * 'a data t
    (** [partition p m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [s] that satisfy the
        predicate [p], and [m2] is the map with all the bindings of
        [s] that do not satisfy [p]. *)

    val cardinal: 'a data t -> int
    (** Return the number of bindings of a map. *)

    val bindings: 'a data t -> (key * 'a data) list
    (** Return the list of all bindings of the given map.
        The returned list is sorted in increasing order with respect
        to the ordering [Ord.compare], where [Ord] is the argument
        given to {!Extmap.Make}. *)

    val min_binding: 'a data t -> (key * 'a data)
    (** Return the smallest binding of the given map
        (with respect to the [Ord.compare] ordering), or raise
        [Not_found] if the map is empty. *)

    val max_binding: 'a data t -> (key * 'a data)
    (** Same as {!Extmap.S.min_binding}, but returns the largest
        binding of the given map. *)

    val choose: 'a data t -> (key * 'a data)
    (** Return one binding of the given map, or raise [Not_found] if
        the map is empty. Which binding is chosen is unspecified,
        but equal bindings will be chosen for equal maps. *)

    val split: key -> 'a data t -> 'a data t * 'a data option * 'a data t
    (** [split x m] returns a triple [(l, data, r)], where
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [x];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [x];
          [data] is [None] if [m] contains no binding for [x],
          or [Some v] if [m] binds [v] to [x]. *)

    val find: key -> 'a data t -> 'a data
    (** [find x m] returns the current binding of [x] in [m],
        or raises [Not_found] if no such binding exists. *)

    val map: ('a data -> 'b data) -> 'a data t -> 'b data t
    (** [map f m] returns a map with same domain as [m], where
        the associated value [a] of all bindings of [m] has been
        replaced by the result of the application of [f] to [a].
        The bindings are passed to [f] in increasing order
        with respect to the ordering over the type of the keys. *)

    val mapi: (key -> 'a data -> 'b data) -> 'a data t -> 'b data t
    (** Same as {!Extmap.S.map}, but the function receives as arguments both
        the key and the associated value for each binding of the map. *)

    (** @Added in Why3 *)

    val change :
      ('a data option -> 'a data option) -> key -> 'a data t -> 'a data t
    (** [change f x m] returns a map containing the same bindings as
        [m], except the binding of [x] in [m] is changed from [y] to
        [f (Some y)] if [m] contains a binding of [x], otherwise the
        binding of [x] becomes [f None].

        [change f x m] corresponds to a more efficient way to do
        [match (try f (Some (find x m)) with Not_found -> f None) with
          | None -> m
          | Some v -> add x v] *)

    val add_change : ('b -> 'a data) -> ('b -> 'a data -> 'a data) ->
      key -> 'b -> 'a data t -> 'a data t
    (** [add_change empty add x b m] corresponds to a more efficient
        and simpler way to do
        [change (function
        | None -> Some (empty b)
        | Some l -> Some (add b l)) x m]
    *)

    val union :
      (key -> 'a data -> 'a data -> 'a data option) ->
      'a data t -> 'a data t -> 'a data t
    (** [union f m1 m2] computes a map whose keys is a subset of keys
        of [m1] and of [m2]. If a binding is present in [m1] (resp. [m2])
        and not in [m2] (resp. [m1]) the same binding is present in
        the result. The function [f] is called only in ambiguous cases. *)

   val union_merge:
      (key -> 'a data option -> 'b data -> 'a data option) ->
      'a data t -> 'b data t -> 'a data t
    (** Between union for the first argument and merge for the second
        argument *)

    val inter :
      (key -> 'a data -> 'b data -> 'c data option) ->
      'a data t -> 'b data t -> 'c data t
    (** [inter f m1 m2] computes a map whose keys is a subset of
        the intersection of keys of [m1] and of [m2]. *)

    val diff :
      (key -> 'a data -> 'b data -> 'a data option) ->
      'a data t -> 'b data t -> 'a data t
    (** [diff f m1 m2] computes a map whose keys is a subset of keys
        of [m1]. [f] is applied on key which belongs to [m1] and [m2]
        if [f] returns [None] the binding is removed from [m1],
        otherwise [Some d1] is returned, the key binds to [d1] in [m1] *)

    val submap :
      (key -> 'a data -> 'b data -> bool) ->
      'a data t -> 'b data t -> bool
    (** [submap pr m1 m2] verifies that all the keys in m1 are in m2
        and that for each such binding pr is verified. *)

    val disjoint :
      (key -> 'a data -> 'b data -> bool) ->
      'a data t -> 'b data t -> bool
    (** [disjoint pr m1 m2] verifies that for every common key in m1
        and m2, pr is verified. *)

    val set_union : 'a data t -> 'a data t -> 'a data t
    (** [set_union = union (fun _ x _ -> Some x)] *)

    val set_inter : 'a data t -> 'b data t -> 'a data t
    (** [set_inter = inter (fun _ x _ -> Some x)] *)

    val set_diff : 'a data t -> 'b data t -> 'a data t
    (** [set_diff = diff (fun _ _ _ -> None)] *)

    val set_submap : 'a data t -> 'b data t -> bool
    (** [set_submap = submap (fun _ _ _ -> true)] *)

    val set_disjoint : 'a data t -> 'b data t -> bool
    (** [set_disjoint = disjoint (fun _ _ _ -> false)] *)

    val set_compare : 'a data t -> 'b data t -> int
    (** [set_compare = compare (fun _ _ -> 0)] *)

    val set_equal : 'a data t -> 'b data t -> bool
    (** [set_equal = equal (fun _ _ -> true)] *)

    val find_def : 'a data -> key -> 'a data t -> 'a data
    (** [find_def x d m] returns the current binding of [x] in [m],
        or return [d] if no such binding exists. *)

    val find_opt : key -> 'a data t -> 'a data option
    (** [find_opt x m] returns the [Some] of the current binding
        of [x] in [m], or return [None] if no such binding exists. *)

    val find_exn : exn -> key -> 'a data t -> 'a data
    (** [find_exn exn x d m] returns the current binding
        of [x] in [m], or raise [exn] if no such binding exists. *)

    val find_remove: key -> 'a data t -> 'a data t * 'a data option
    (** [find_remove x m] returns the map without the binding and the
        binding *)

    val add_opt: key -> 'a data option -> 'a data t -> 'a data t
     (** Just add or remove the binding if a data is or not given *)

    val find_smaller_opt : key -> 'a data t -> (key * 'a data) option
      (** return the binding of the maximum key smaller than the given key. *)

    val map_filter: ('a data -> 'b data option) -> 'a data t -> 'b data t
    (** Same as {!Extmap.S.map}, but may remove bindings. *)

    val mapi_filter:
      (key -> 'a data -> 'b data option) -> 'a data t -> 'b data t
    (** Same as {!Extmap.S.mapi}, but may remove bindings. *)

    val mapi_fold:
      (key -> 'a data -> 'acc -> 'acc * 'b data) ->
      'a data t -> 'acc -> 'acc * 'b data t
    (** fold and map at the same time *)

    val mapi_filter_fold:
      (key -> 'a data -> 'acc -> 'acc * 'b data option) ->
      'a data t -> 'acc -> 'acc * 'b data t
    (** Same as {!Extmap.S.mapi_fold}, but may remove bindings. *)

    val fold_left:
      ('b -> key -> 'a data -> 'b) -> 'b -> 'a data t -> 'b
    (** same as {!fold} but in the order of {!List.fold_left} *)

    val fold2_inter:
      (key -> 'a data -> 'b data -> 'c -> 'c) ->
      'a data t -> 'b data t -> 'c -> 'c
    (** fold the common keys of two map at the same time *)

    val fold2_union:
      (key -> 'a data option -> 'b data option -> 'c -> 'c) ->
      'a data t -> 'b data t -> 'c -> 'c
    (** fold the keys which appear in one of the two maps *)

    val fold_decr:
      ('b -> key -> 'a data -> 'b) -> 'b -> 'a data t -> 'b
    (** same as {!fold_left} but in decreasing order *)

    val translate :
      (key -> key) -> 'a data t -> 'a data t
    (** [translate f m] translates the keys in the map [m] by the
        function [f]. [f] must be strictly monotone on the key of [m].
        Otherwise it raises invalid_arg *)

    val add_new : exn -> key -> 'a data -> 'a data t -> 'a data t
    (** [add_new e x v m] binds [x] to [v] in [m] if [x] is not bound,
        and raises [e] otherwise. *)

    val keys: 'a data t -> key list
    (** Return the list of all keys of the given map.
        The returned list is sorted in increasing order with respect
        to the ordering [Ord.compare], where [Ord] is the argument
        given to {!Extmap.Make}. *)

    val values: 'a data t -> 'a data list
    (** Return the list of all values of the given map.
        The returned list is sorted in increasing order with respect
        to the ordering [Ord.compare] of the keys, where [Ord] is the argument
        given to {!Extmap.Make}. *)

    val of_list: (key * 'a data) list -> 'a data t
    (** construct a map from a pair of bindings *)

    val is_num_elt : int -> 'a data t -> bool
    (** check if the map has the given number of elements *)

    val choose_rnd : (unit -> bool) -> 'a data t -> key * 'a data
    (** choose an element randomly (but non-uniformly), the given function is
        the random generator *)

    type 'a enumeration
    (** enumeration: zipper style *)

    val val_enum : 'a data enumeration -> (key * 'a data) option
    (** get the current key value pair of the enumeration, return None
        if the enumeration reach the end *)

    val start_enum : 'a data t -> 'a data enumeration
    (** start the enumeration of the given map *)

    val next_enum : 'a data enumeration -> 'a data enumeration
    (** get the next step of the enumeration *)

    val start_ge_enum : key -> 'a data t -> 'a data enumeration
    (** start the enumeration of the given map at the first key which
        is greater or equal than the given one *)

    val next_ge_enum : key -> 'a data enumeration -> 'a data enumeration
    (** get the next (or same) step of the enumeration which key is
        greater or equal to the given key *)

    val check_invariant: 'a data t -> bool
      (** Check if the internal invariant are verified.
          Only for debugging the datastructure*)
  end

type 'a pvar = 'a
module type PMap = Map with type 'a data := 'a pvar

module type Map_hashcons = sig
  include Map

  type 'a poly

  val nt: 'a data t -> 'a data poly
  (** constant time *)
  val rebuild: 'a data poly -> 'a data t
  (** linear *)

  (** comparison induced by hashconsing *)
  val compare_t: 'a data t -> 'a data t -> int
  val equal_t: 'a data t -> 'a data t -> bool

  (** with NT.t *)
  val inter_nt :
    (key -> 'a data -> 'b -> 'a data) -> 'a data t -> 'b poly -> 'a data t
  val interf_nt : (key -> 'a data -> 'b -> 'a data option) ->
    'a data t -> 'b poly -> 'a data t
  val set_inter_nt : 'a data t -> 'b poly -> 'a data t
end


module type Gen_Map_hashcons = sig

  module NT: Map with type 'a data = 'a
  (** A Map without hashconsing, can be also used for reading an
      hashconsed tree in a polymorphic way, but without using the
      hashconsed property *)


  module Make(Data: HashType):
    Map_hashcons with type 'a data = Data.t
                and type 'a poly := 'a NT.t
                and type key = NT.key
end


type 'a punit = unit
module type MapUnit = sig
  include Map with type 'a data := 'a punit
end

(** Output signature of the functor {!Extset.Make}. *)
module type Set =
  sig
    module M : MapUnit
    (** The module of association tables over [elt]. *)

    type elt = M.key
    (** The type of set elements. *)

    type t = unit M.t
    (** The type of sets of type [elt]. *)

    val pp: t Pp.pp

    val empty: t
    (** The empty set. *)

    val is_empty: t -> bool
    (** Test whether a set is empty or not. *)

    val mem: elt -> t -> bool
    (** [mem x s] returns [true] if [s] contains [x],
        and [false] otherwise. *)

    val add: elt -> t -> t
    (** [add x s] returns a set containing the same elements as
        [s], plus [x]. *)

    val singleton: elt -> t
    (** [singleton x] returns the one-element set that contains [x]. *)

    val remove: elt -> t -> t
    (** [remove x s] returns a set containing the same elements as [s],
        except for [x]. *)

    val merge: (elt -> bool -> bool -> bool) -> t -> t -> t
    (** [merge f s1 s2] computes a set whose elts is a subset of elts
        of [s1] and of [s2]. The presence of each such element is
        determined with the function [f]. *)

    val compare: t -> t -> int
    (** Total ordering between sets. *)

    val equal: t -> t -> bool
    (** [equal s1 s2] tests whether the sets [s1] and [s2] are equal. *)

    val subset: t -> t -> bool
    (** [subset s1 s2] tests whether the set [s1] is a subset of [s2]. *)

    val disjoint: t -> t -> bool
    (** [disjoint s1 s2] tests whether the sets [s1] and [s2]
        are disjoint. *)

    val iter: (elt -> unit) -> t -> unit
    (** [iter f s] applies [f] to all elements of [s].
        The elements are passed to [f] in increasing order with respect
        to the ordering over the type of the elts. *)

    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f s a] computes [(f eN ... (f e1 a)...)],
        where [e1 ... eN] are the element of [s] in increasing order. *)

    val for_all: (elt -> bool) -> t -> bool
    (** [for_all p s] checks if all the elements of [s] satisfy
        the predicate [p]. *)

    val exists: (elt -> bool) -> t -> bool
    (** [exists p s] checks if at least one element of [s] satisfies
        the predicate [p]. *)

    val filter: (elt -> bool) -> t -> t
    (** [filter p s] returns the set with all the elements of [s]
        that satisfy predicate [p]. *)

    val partition: (elt -> bool) -> t -> t * t
    (** [partition p s] returns a pair of sets [(s1, s2)], where
        [s1] contains all the elements of [s] that satisfy the
        predicate [p], and [s2] is the map with all the elements
        of [s] that do not satisfy [p]. *)

    val cardinal: t -> int
    (** Return the number of elements in a set. *)

    val elements: t -> elt list
    (** Return the list of all elements of the given set.
        The returned list is sorted in increasing order. *)

    val min_elt: t -> elt
    (** Return the smallest element of the given set or raise
        [Not_found] if the set is empty. *)

    val max_elt: t -> elt
    (** Return the largest element of the given set or raise
        [Not_found] if the set is empty. *)

    val choose: t -> elt
    (** Return one element of the given set, or raise [Not_found] if
        the set is empty. Which element is chosen is unspecified,
        but equal elements will be chosen for equal sets. *)

    val split: elt -> t -> t * bool * t
    (** [split x s] returns a triple [(l, mem, r)], where
        [l] is the set with all the elements of [s] that are
        strictly less than [x];
        [r] is the set with all the elements of [s] that are
        strictly greater than [x];
        [mem] is [true] if [x] belongs to [s] and [false] otherwise. *)

    val change : (bool -> bool) -> elt -> t -> t
    (** [change f x s] returns a set containing the same elements as
        [s], except [x] which is added to [s] if [f (mem x s)] returns
        [true] and removed otherwise. *)

    val union : t -> t -> t
    (** [union f s1 s2] computes the union of two sets *)

    val inter : t -> t -> t
    (** [inter f s1 s2] computes the intersection of two sets *)

    val diff : t -> t -> t
    (** [diff f s1 s2] computes the difference of two sets *)

    val fold_left : ('b -> elt -> 'b) -> 'b -> t -> 'b
    (** same as {!fold} but in the order of {!List.fold_left} *)

    val fold2_inter : (elt -> 'a -> 'a) -> t -> t -> 'a -> 'a
    (** [fold2_inter f s1 s2 a] computes [(f eN ... (f e1 a) ...)],
        where [e1 ... eN] are the elements of [inter s1 s2]
        in increasing order. *)

    val fold2_union : (elt -> 'a -> 'a) -> t -> t -> 'a -> 'a
    (** [fold2_union f s1 s2 a] computes [(f eN ... (f e1 a) ...)],
        where [e1 ... eN] are the elements of [union s1 s2]
        in increasing order. *)

    val translate : (elt -> elt) -> t -> t
    (** [translate f s] translates the elements in the set [s] by the
        function [f]. [f] must be strictly monotone on the elements of [s].
        Otherwise it raises invalid_arg *)

    val add_new : exn -> elt -> t -> t
    (** [add_new e x s] adds [x] to [s] if [s] does not contain [x],
        and raises [e] otherwise. *)

    val is_num_elt : int -> t -> bool
    (** check if the map has the given number of elements *)

    val of_list: elt list -> t
    (** construct a set from a list of elements *)
  end

module type Set_hashcons = sig
  include Set

  type 'a poly

  val nt: t -> unit poly
  (** constant time *)
  val rebuild: unit poly -> t
  (** linear *)

  (** comparison induced by hashconsing *)
  val compare_t: t -> t -> int
  val equal_t: t -> t -> bool

  (** with NT.t *)
  val inter_nt : t -> 'b poly -> t
end
