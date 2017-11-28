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

exception UnregisteredKey
exception AlreadyRegisteredKey

exception BadCoercion

type (_,_) eq = Eq : ('a,'a) eq


module type Registry = sig
  type 'a key
  type 'a data

  val register: 'a data -> unit
  val check_is_registered : 'a key -> unit
  val is_well_initialized : unit -> bool
  val get : 'a key -> 'a data
  val print : 'a key -> 'a Pp.pp

end

module type Key = sig
  (** Key with arity 1 *)

  module K: Datatype
  type 'a t = private K.t

  val pp: 'a t Pp.pp
  val compare: 'a t -> 'b t -> int
  val equal: 'a t -> 'b t -> bool
  val hash : 'a t -> int
  val tag: 'a t -> int

  type iter = {iter : 'a. 'a t -> unit}
  val iter : iter -> unit
  val hint_size : unit -> int

  module Eq: sig
    val eq_type : 'a t -> 'b t -> ('a,'b) eq option
    (** If the two arguments are physically identical then an equality witness
        between the types is returned *)

    val coerce_type : 'a t -> 'b t -> ('a,'b) eq
    (** If the two arguments are physically identical then an equality witness
        between the types is returned otherwise
        the exception BadCoercion is raised  *)

    val coerce : 'a t -> 'b t -> 'a -> 'b
    (** If the two arguments are physically identical then covnert the
        argument otherwise taise BadCoercion *)

  end
  val create_key: string -> 'a t

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


module Make_key(X:sig end): Key = struct
  module K = Strings.Fresh(struct end)

  type 'a t = K.t (* >= 0 *)
  let pp fmt x = K.pp fmt x
  let compare x y   = K.compare x y
  let equal x y   = K.equal x y
  let hash  x     = K.hash x
  let tag (x:K.t) = (x:>int)

  type iter = {iter : 'a. 'a t -> unit}
  let iter f = K.iter f.iter
  let hint_size = K.hint_size

  let create_key s = K.create s

  (** the 'a k can be used as equality witness because K gives fresh values *)
  module Eq = struct
    let eq_type :
      type a b. a t -> b t -> (a,b) eq option =
      fun a b ->
        if equal a b
        then Some ((Obj.magic (Eq : (a,a) eq)) : (a,b) eq)
        else None

    let coerce_type :
      type a b. a t -> b t -> (a,b) eq =
      fun a b ->
        if equal a b
        then ((Obj.magic (Eq : (a,a) eq)) : (a,b) eq)
        else raise BadCoercion

    let coerce (type a) (type b) (a:a t) (b:b t) (x:a) : b =
      match coerce_type a b with
      | (Eq:(a,b) eq) -> x
  end
  module MkVector(D:sig type ('a,'b) t end) =
    Vector_hetero.Make1(struct type nonrec 'a t = 'a t end)(D)
  module MkMap(D:sig type ('a,'b) t end) =
    Intmap_hetero.Make1(struct type nonrec 'a t = 'a t end)(D)
  module Vector =
    Vector_hetero.RMake1(struct type nonrec 'a t = 'a t end)
  module VectorH =
    Vector_hetero.TMake1(struct type nonrec 'a t = 'a t end)
  module M =
    Intmap_hetero.RMake1(struct type nonrec 'a t = 'a t end)

  module Make_Registry(S:sig
      type 'a data
      val pp: 'a data -> 'a Pp.pp
      val key: 'a data -> 'a t
    end) = struct

    type 'a data = 'a S.data

    module V = MkVector(struct type ('a,'unedeed) t = 'a S.data end)

    let registry : unit V.t = V.create 8

    let register data =
      let key = S.key data in
        V.inc_size key registry;
        assert (if not (V.is_uninitialized registry key)
                then raise AlreadyRegisteredKey else true);
        V.set registry key data

    let check_is_registered data =
      assert (if V.is_uninitialized registry data
              then raise UnregisteredKey else true)

    let is_well_initialized () =
      let well_initialized = ref true in
      iter {iter = fun data ->
          if V.is_uninitialized registry data then begin
            Format.eprintf "[Warning] %a is not registered" pp data;
            well_initialized := false;
          end};
      !well_initialized

    let is_registered dom =
      V.is_uninitialized registry dom

    let get k =
      check_is_registered k;
      V.get registry k

    let print (type a) (k : a t) fmt s =
      let data = get k in
      (S.pp data) fmt s
  end
end

module type Registry2 = sig
  type ('k,'d) key
  type ('k,'d) data

  val register: ('k,'d) data -> unit
  val check_is_registered : ('k,'d) key -> unit
  val is_well_initialized : unit -> bool
  val get : ('k,'d) key -> ('k,'d) data
  val printk : ('k,'d) key -> 'k Pp.pp
  val printd : ('k,'d) key -> 'd Pp.pp

end

module type Key2 = sig
  (** Key with arity 2 *)

  module K: Datatype
  type ('k,'d) t = private K.t
  (** kind of daemon for semantic value of type 'a *)
  val pp: ('k,'d) t Pp.pp
  val equal: ('k1,'d1) t -> ('k2,'d2) t -> bool
  val hash : ('k,'d) t -> int

  type iter = {iter : 'k 'd. ('k,'d) t -> unit}
  val iter : iter -> unit

  val create_key: string -> ('k,'d) t

  module Eq: sig
    val eq_type : ('a1,'b1) t -> ('a2,'b2) t
      -> (('a1,'a2) eq * ('b1,'b2) eq) option
    (** If the two arguments are physically identical then an equality witness
        between the types is returned *)

    val coerce_type : ('a1,'b1) t -> ('a2,'b2) t
      -> ('a1,'a2) eq * ('b1,'b2) eq
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

module Make_key2(X:sig end) : Key2 = struct
  module K = Strings.Fresh(struct end)

  type ('k,'d) t = K.t (* >= 0 *)
  let pp fmt x = K.pp fmt x
  let equal = K.equal
  let hash  x     = K.hash x

  type iter = {iter : 'k 'd. ('k,'d) t -> unit}
  let iter f = K.iter f.iter

  let create_key s = K.create s

  (** the ('k,'d) k can be used as equality witness because K gives
      fresh values *)
  module Eq = struct

    let eq_type :
      type a1 b1 a2 b2. (a1,b1) t -> (a2,b2) t
      -> ((a1,a2) eq * (b1,b2) eq) option =
      fun a b ->
        if equal a b
        then let eq1 = (Obj.magic (Eq : (a1,a1) eq) : (a1,a2) eq) in
          let eq2 = (Obj.magic (Eq : (b1,b1) eq) : (b1,b2) eq) in
          Some (eq1,eq2)
        else None

    let coerce_type :
      type a1 b1 a2 b2. (a1,b1) t -> (a2,b2) t
      -> ((a1,a2) eq * (b1,b2) eq) =
      fun a b ->
        if equal a b
        then let eq1 = (Obj.magic (Eq : (a1,a1) eq) : (a1,a2) eq) in
          let eq2 = (Obj.magic (Eq : (b1,b1) eq) : (b1,b2) eq) in
          (eq1,eq2)
        else raise BadCoercion

  end
  module MkVector(D:sig type ('k,'d,'b) t end) =
    Vector_hetero.Make2(struct type nonrec ('k,'d) t = ('k,'d) t end)(D)

  module Make_Registry(S:sig
      type ('k,'d) data
      val ppk: ('k,'d) data -> 'k Pp.pp
      val ppd: ('k,'d) data -> 'd Pp.pp
      val key: ('k,'d) data -> ('k,'d) t
    end) = struct

    type ('k,'d) data = ('k,'d) S.data

    module V = MkVector(struct type ('k,'d,'unedeed) t = ('k,'d) S.data end)

    let registry : unit V.t = V.create 8

    let register data =
      let key = S.key data in
        V.inc_size key registry;
        assert (if not (V.is_uninitialized registry key)
                then raise AlreadyRegisteredKey else true);
        V.set registry key data

    let check_is_registered data =
      assert (if V.is_uninitialized registry data
              then raise UnregisteredKey else true)

    let is_well_initialized () =
      let well_initialized = ref true in
      iter {iter = fun data ->
          if V.is_uninitialized registry data then begin
            Format.eprintf "[Warning] %a is not registered" pp data;
            well_initialized := false;
          end};
      !well_initialized

    let is_registered dom =
      V.is_uninitialized registry dom

    let get k =
      check_is_registered k;
      V.get registry k

    let printk (type k) (type d) (k : (k,d) t) fmt s =
      let data = get k in
      (S.ppk data) fmt s
    let printd (type k) (type d) (k : (k,d) t) fmt s =
      let data = get k in
      (S.ppd data) fmt s
  end
end
