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


type statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array
}

val randomize : unit -> unit
(** After a call to [Hashtbl.randomize()], hash tables are created in
    randomized mode by default: {!Hashtbl.create} returns randomized
    hash tables, unless the [~random:false] optional parameter is given.
    The same effect can be achieved by setting the [R] parameter in
    the [OCAMLRUNPARAM] environment variable.

    It is recommended that applications or Web frameworks that need to
    protect themselves against the denial-of-service attack described
    in {!Hashtbl.create} call [Hashtbl.randomize()] at initialization
    time.

    Note that once [Hashtbl.randomize()] was called, there is no way
    to revert to the non-randomized default behavior of {!Hashtbl.create}.
    This is intentional.  Non-randomized hash tables can still be
    created using [Hashtbl.create ~random:false].

    @since 4.00.0 *)

module type Hashtbl =
sig

val hash : 'a -> int


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
  (** return the first binding or the given value if none found *)

  val find_opt : 'a t -> key -> 'a option
  (** return the first binding or None if none found *)

  val find_exn : 'a t -> exn -> key -> 'a
  (** return the first binding or raise the given exception if none found *)

  val mapi : (key -> 'a -> 'a) -> 'a t -> unit
  (** change the value of all the key,
      don't modify the table during this run *)

  val memo : int -> (key -> 'a) -> key -> 'a
  (** convenience function, memoize a function *)

  val is_empty : 'a t -> bool
  (** test if the hashtbl is empty *)

  val remove_all: 'a t -> key -> unit
  (** remove_all binding *)

  val change   : ('a option -> 'a option) -> 'a t -> key -> unit
  val add_new  : exn -> 'a t -> key -> 'a -> unit

  end
(** The output signature of the functor {!Hashtbl.Make}. *)

module Make (H : HashedType) : S with type key = H.t
(** Functor building an implementation of the hashtable structure.
    The functor [Hashtbl.Make] returns a structure containing
    a type [key] of keys and a type ['a t] of hash tables
    associating data of type ['a] to keys of type [key].
    The operations perform similarly to those of the generic
    interface, but use the hashing and equality functions
    specified in the functor argument [H] instead of generic
    equality and hashing.  Since the hash function is not seeded,
    the [create] operation of the result structure always returns
    non-randomized hash tables. *)

module type Private =
sig
  (** Private Hashtbl *)
  type 'a t
  type key

  val find : 'a t -> key -> 'a
  (** Same as {!Hashtbl.find} *)

  val find_def : 'a t -> 'a -> key -> 'a
  (** return the first binding or the given value if none found *)

  val find_opt : 'a t -> key -> 'a option
  (** return the first binding or None if none found *)

  val find_exn : 'a t -> exn -> key -> 'a
  (** return the first binding or raise the given exception if none found *)

  val iter : (key -> 'a -> unit) -> 'a t -> unit
  (** Same as {!Hashtbl.iter} *)

  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  (** Same as {!Hashtbl.fold} *)

  val mem : 'a t -> key -> bool
  (** Same as {!Hashtbl.mem} *)

  val length : 'a t -> int
  (** Same as {!Hashtbl.length} *)

  val is_empty : 'a t -> bool
  (** test if the hashtbl is empty *)
end

end

module Hashtbl : Hashtbl
