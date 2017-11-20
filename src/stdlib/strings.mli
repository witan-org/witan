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

(** Useful functions on string *)

val rev_split : string -> char -> string list

val ends_with : string -> string -> bool
(** test if a string ends with another *)

val pad_right : char -> string -> int -> string
(** chop or pad the given string on the right up to the given length *)



module Hashcons :
 sig
  type t = private int
  include Stdlib.Datatype with type t := t
  val make: string -> t (** hashcons *)
  val fresh: string -> t (** always fresh *)
  val view: t -> string
  val tag: t -> int
  val pp: t Pp.pp
end

module Make (X : sig end): module type of Hashcons

module type Fresh = sig
  type t = private int
  include Stdlib.Datatype with type t := t
  val create: string -> t
  val iter: (t -> unit) -> unit
  val hint_size: unit -> int
  val rename: t -> string -> unit
    (** to use with care *)
end

module Fresh (X : sig end) : Fresh

val find_new_name: int Stdlib.DStr.H.t -> string -> string
