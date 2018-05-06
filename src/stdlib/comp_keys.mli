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

(** Key generators *)

(** Keys are the main programming tools used for implementing
    extensible types (sem, value, dom, pexp, ...) *)

open Witan_popop_lib
open Stdlib

open Std

module type NamedType = sig
  type t
  val name : string
end

module type NamedType2 = sig
  type t
  type d
  val name : string
end

(** {2 Exceptions} *)

(** {2 Type comparison and coercion } *)

exception BadCoercion
(** Raised when making a bad coercion *)

(* type (_,_) eq = Eq : ('a,'a) eq
 * (\** Proof of type equality *\) *)

module type Registry = sig
  type 'a key
  type 'a data

  val register: 'a data -> unit
  val check_is_registered : 'a key -> unit
  val is_well_initialized : unit -> bool
  val get : 'a key -> 'a data
  val print : 'a key -> 'a Pp.pp


  (** the key shouldn't be used before its registration and shouldn't be
      registered again *)
  exception UnregisteredKey : 'a key -> exn
  exception AlreadyRegisteredKey : 'a key -> exn

end

module type Key = sig
  (** Key with arity 1 *)

  module K: Datatype
  type 'a t (* = private K.t *)

  val pp: 'a t Pp.pp
  val compare: 'a t -> 'b t -> int
  val equal: 'a t -> 'b t -> bool
  val hash : 'a t -> int
  val tag: 'a t -> int

  type iter = {iter : 'a. 'a t -> unit}
  val iter : iter -> unit
  type 'b fold = {fold : 'a. 'a t -> 'b -> 'b}
  val fold : 'b fold -> 'b -> 'b
  val hint_size : unit -> int

  module Eq: sig
    val eq_type : 'a t -> 'b t -> ('a,'b) Poly.iseq
    (** If the two arguments are physically identical then an equality witness
        between the types is returned *)

    val coerce_type : 'a t -> 'b t -> ('a,'b) Poly.eq
    (** If the two arguments are physically identical then an equality witness
        between the types is returned otherwise
        the exception BadCoercion is raised  *)

    val coerce : 'a t -> 'b t -> 'a -> 'b
    (** If the two arguments are physically identical then covnert the
        argument otherwise taise BadCoercion *)

  end
  val create_key: (module NamedType with type t = 'a) -> 'a t

  module MkVector(D:sig type ('a,'b) t end)
    : Vector_hetero.S1 with
                         type 'a key = 'a t and type ('a,'b) data = ('a,'b) D.t

  module MkMap(D:sig type ('a,'b) t end)
    : Intmap_hetero.S1 with
                         type 'a key = 'a t and type ('a,'b) data = ('a,'b) D.t

  module Vector  : Vector_hetero.R1 with type 'a key = 'a t
  module VectorH : Vector_hetero.T1 with type 'a key = 'a t
  module M : Intmap_hetero.R1 with type 'a key = 'a t
  module Make_Registry(S:sig
      type 'a data
      val pp: 'a data -> 'a Pp.pp
      val key: 'a data -> 'a t
    end) : Registry with type 'a key := 'a t and type 'a data = 'a S.data
end

module Make_key(X:sig end) : Key

module type Registry2 = sig
  type ('k,'d) key
  type ('k,'d) data

  val register: ('k,'d) data -> unit
  val check_is_registered : ('k,'d) key -> unit
  val is_well_initialized : unit -> bool
  val get : ('k,'d) key -> ('k,'d) data
  val printk : ('k,'d) key -> 'k Pp.pp
  val printd : ('k,'d) key -> 'd Pp.pp

  exception UnregisteredKey : ('a,'b) key -> exn
  exception AlreadyRegisteredKey : ('a,'b) key -> exn

end

module type Key2 = sig
  (** Key with arity 2 *)

  module K: Datatype
  type ('k,'d) t (* = private K.t *)
  (** kind of daemon for semantic value of type 'a *)
  val pp: ('k,'d) t Pp.pp
  val equal: ('k1,'d1) t -> ('k2,'d2) t -> bool
  val hash : ('k,'d) t -> int

  type iter = {iter : 'k 'd. ('k,'d) t -> unit}
  val iter : iter -> unit

  val create_key: (module NamedType2 with type t = 'a1
                                      and type d = 'a2)
                  -> ('a1,'a2) t

  module Eq: sig
    val eq_type : ('a1,'b1) t -> ('a2,'b2) t
      -> (('a1,'a2) Poly.eq * ('b1,'b2) Poly.eq) option
    (** If the two arguments are physically identical then an equality witness
        between the types is returned *)

    val coerce_type : ('a1,'b1) t -> ('a2,'b2) t
      -> ('a1,'a2) Poly.eq * ('b1,'b2) Poly.eq
      (** If the two arguments are physically identical then an equality witness
          between the types is returned otherwise
          the exception BadCoercion is raised  *)
  end
  module MkVector(D:sig type ('k,'d,'b) t end)
    : Vector_hetero.S2 with type ('k,'d) key = ('k,'d) t
                        and type ('k,'d,'b) data = ('k,'d,'b) D.t
  module Make_Registry(S:sig
      type ('k,'d) data
      val ppk: ('k,'d) data -> 'k Pp.pp
      val ppd: ('k,'d) data -> 'd Pp.pp
      val key: ('k,'d) data -> ('k,'d) t
    end) : Registry2 with type ('k,'d) key := ('k,'d) t and type ('k,'d) data = ('k,'d) S.data

end

module Make_key2(X:sig end): Key2
