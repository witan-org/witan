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

open Stdlib

exception BadCoercion

type (_,_) eq = Eq : ('a,'a) eq

module type Key = sig

  module K: Datatype
  type 'a k = private K.t
  val pp: 'a k Pp.pp
  val compare: 'a k -> 'b k -> int
  val equal: 'a k -> 'b k -> bool
  val hash : 'a k -> int
  val tag: 'a k -> int

  type iter = {iter : 'a. 'a k -> unit}
  val iter : iter -> unit
  val hint_size : unit -> int

  module Eq: sig
    val eq_type : 'a k -> 'b k -> ('a,'b) eq option
    (** If the two arguments are physically identical then an equality witness
        between the types is returned *)

    val coerce_type : 'a k -> 'b k -> ('a,'b) eq
    (** If the two arguments are physically identical then an equality witness
        between the types is returned otherwise
        the exception BadCoercion is raised  *)

    val coerce : 'a k -> 'b k -> 'a -> 'b
    (** If the two arguments are physically identical then covnert the
        argument otherwise taise BadCoercion *)

  end
  val create_key: string -> 'a k

  module MkVector(D:sig type ('a,'b) t end)
    : Vector_hetero.S1 with
                         type 'a key = 'a k and type ('a,'b) data = ('a,'b) D.t

  module MkMap(D:sig type ('a,'b) t end)
    : Intmap_hetero.S1 with
                         type 'a key = 'a k and type ('a,'b) data = ('a,'b) D.t

  module Vector : Vector_hetero.R1 with type 'a key = 'a k
  module VectorH : Vector_hetero.T1 with type 'a key = 'a k
  module M : Intmap_hetero.R1 with type 'a key = 'a k
end

module Make_key(X:sig end): Key = struct
  module K = Strings.Fresh(struct end)

  type 'a k = K.t (* >= 0 *)
  let pp fmt x = K.pp fmt x
  let compare x y   = K.compare x y
  let equal x y   = K.equal x y
  let hash  x     = K.hash x
  let tag (x:K.t) = (x:>int)

  type iter = {iter : 'a. 'a k -> unit}
  let iter f = K.iter f.iter
  let hint_size = K.hint_size

  let create_key s = K.create s

  (** the 'a k can be used as equality witness because K gives fresh values *)
  module Eq = struct
    let eq_type :
      type a b. a k -> b k -> (a,b) eq option =
      fun a b ->
        if equal a b
        then Some ((Obj.magic (Eq : (a,a) eq)) : (a,b) eq)
        else None

    let coerce_type :
      type a b. a k -> b k -> (a,b) eq =
      fun a b ->
        if equal a b
        then ((Obj.magic (Eq : (a,a) eq)) : (a,b) eq)
        else raise BadCoercion

    let coerce (type a) (type b) (a:a k) (b:b k) (x:a) : b =
      match coerce_type a b with
      | (Eq:(a,b) eq) -> x
  end
  module MkVector(D:sig type ('a,'b) t end) =
    Vector_hetero.Make1(struct type 'a t = 'a k end)(D)
  module MkMap(D:sig type ('a,'b) t end) =
    Intmap_hetero.Make1(struct type 'a t = 'a k end)(D)
  module Vector =
    Vector_hetero.RMake1(struct type 'a t = 'a k end)
  module VectorH =
    Vector_hetero.TMake1(struct type 'a t = 'a k end)
  module M =
    Intmap_hetero.RMake1(struct type 'a t = 'a k end)

end


module type Key2 = sig
  module K: Datatype
  type ('k,'d) k = private K.t
  (** kind of daemon for semantic value of type 'a *)
  val pp: ('k,'d) k Pp.pp
  val equal: ('k1,'d1) k -> ('k2,'d2) k -> bool
  val hash : ('k,'d) k -> int

  type iter = {iter : 'k 'd. ('k,'d) k -> unit}
  val iter : iter -> unit

  val create_key: string -> ('k,'d) k

  module Eq: sig
    val eq_type : ('a1,'b1) k -> ('a2,'b2) k
      -> (('a1,'a2) eq * ('b1,'b2) eq) option
    (** If the two arguments are physically identical then an equality witness
        between the types is returned *)

    val coerce_type : ('a1,'b1) k -> ('a2,'b2) k
      -> ('a1,'a2) eq * ('b1,'b2) eq
      (** If the two arguments are physically identical then an equality witness
          between the types is returned otherwise
          the exception BadCoercion is raised  *)
  end
  module MkVector(D:sig type ('k,'d,'b) t end)
    : Vector_hetero.S2 with type ('k,'d) key = ('k,'d) k
                       and type ('k,'d,'b) data = ('k,'d,'b) D.t
end

module Make_key2(X:sig end) : Key2 = struct
  module K = Strings.Fresh(struct end)

  type ('k,'d) k = K.t (* >= 0 *)
  let pp fmt x = K.pp fmt x
  let equal = K.equal
  let hash  x     = K.hash x

  type iter = {iter : 'k 'd. ('k,'d) k -> unit}
  let iter f = K.iter f.iter

  let create_key s = K.create s

  (** the ('k,'d) k can be used as equality witness because K gives
      fresh values *)
  module Eq = struct

    let eq_type :
      type a1 b1 a2 b2. (a1,b1) k -> (a2,b2) k
      -> ((a1,a2) eq * (b1,b2) eq) option =
      fun a b ->
        if equal a b
        then let eq1 = (Obj.magic (Eq : (a1,a1) eq) : (a1,a2) eq) in
          let eq2 = (Obj.magic (Eq : (b1,b1) eq) : (b1,b2) eq) in
          Some (eq1,eq2)
        else None

    let coerce_type :
      type a1 b1 a2 b2. (a1,b1) k -> (a2,b2) k
      -> ((a1,a2) eq * (b1,b2) eq) =
      fun a b ->
        if equal a b
        then let eq1 = (Obj.magic (Eq : (a1,a1) eq) : (a1,a2) eq) in
          let eq2 = (Obj.magic (Eq : (b1,b1) eq) : (b1,b2) eq) in
          (eq1,eq2)
        else raise BadCoercion

  end
  module MkVector(D:sig type ('k,'d,'b) t end) =
    Vector_hetero.Make2(struct type ('k,'d) t = ('k,'d) k end)(D)
end
