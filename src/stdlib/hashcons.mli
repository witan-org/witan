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

(** Hash tables for hash consing

    Hash consing tables are using weak pointers, so that values that are no
    more referenced from anywhere else can be erased by the GC.

    Look in src/core/term.ml for usage examples. *)

(** Values to be hash-consed must implement signature [HashedType] below.
    Type [t] is the type of values to be hash-consed.
    The user must provide an equality and a hash function over type [t],
    as well as a function [tag] to build a new value of type [t] from
    an old one and a unique integer tag. *)

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
    include Stdlib.Datatype

    val hashcons : t -> t
      (** [hashcons n] hash-cons the value [n] i.e. returns
          any existing value in the table equal to [n], if any;
          otherwise, creates a new value with function [tag], stores it
          in the table and returns it.
      *)

    (** Versions where you don't create a value before modifying it *)
    val hashcons0: (int -> t) -> t
    val hashcons1: (int -> 'a -> t) -> 'a -> t
    val hashcons2: (int -> 'a -> 'b -> t) -> 'a -> 'b -> t
    val hashcons3: (int -> 'a -> 'b -> 'c -> t) -> 'a -> 'b -> 'c -> t

    val iter : (t -> unit) -> unit
      (** [iter f] iterates [f] over all elements of the table . *)

    type cat
    val stats : cat -> int * int * int * int * int * int
      (** Return statistics on the table.  The numbers are, in order:
          table length, number of entries, sum of bucket lengths,
          smallest bucket length, median bucket length, biggest
          bucket length. *)

    val fresh_tag: unit -> int
      (** get a fresh tag *)
  end

module Make(H : HashedType) : (S with type t = H.t
                                  and type cat := unit)

module MakeCat(H : sig include HashedType val category: t -> int end) :
  (sig
    include S with type t = H.t and type cat := int
    val new_category: int -> unit
    val iter_cat: (t -> unit) -> int -> unit
  end)


module MakeTag(H : sig include HashedType
                 val next_tag: unit -> int
                 val incr_tag: unit -> unit
               end) :
  (S with type t = H.t and type cat := unit)

(* helpers *)

val combine : int -> int -> int
val combine2 : int -> int -> int -> int
val combine3 : int -> int -> int -> int -> int
val combine_list : ('a -> int) -> int -> 'a list -> int
val combine_option : ('a -> int) -> 'a option -> int
val combine_pair : ('a -> int) -> ('b -> int) -> 'a * 'b -> int
