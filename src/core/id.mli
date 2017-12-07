
(** Witan identifiers.

    Generic identifiers for use throughout witan. Each id holds
    a value, understood to be its 'type'.
*)

(** {2 Generic identifiers} *)

type 'a t
(** Abstract type of identifiers. An ['a t] is a identifier holding
    a value of caml-type ['a].*)

val hash: 'a t -> int
val equal : 'a t -> 'a t -> bool
val compare : 'a t -> 'a t -> int
(** Usual functions. *)

val print : Format.formatter -> 'a t -> unit
(** Print an id. *)

val mk : string -> 'a -> 'a t
(** Create a named id, holding an arbitrary value. Each id generated
    by this function is fresh (i.e. different from all other id, even
    if they share the same name and value). *)

val name : 'a t -> string
(** Return an identifier name. *)

val ty : 'a t -> 'a
(** Returns the type of an identifier. *)


(** {2 Any identifiers} *)

type 'a id = 'a t
(** Type alias. *)

module Any : sig
  (** Any identifiers.

      This module define a thin wrapper around identifiers to
      be able to bundle together ids with different types. *)

  type t
  (** Represents an identifiers with holding a vlaue of an arbitrary caml-type. *)

  val mk : 'a id -> t
  (** Create an any id from a typed id. *)

  val hash : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool
  (** Compare any ids.
      Using inlining, chaining {!mk} with these functions should be
      equivalent (performance-wise) to the type-safe functions on typed
      identifiers. *)

  val print : Format.formatter -> t -> unit
  (** Print any id. *)

end
