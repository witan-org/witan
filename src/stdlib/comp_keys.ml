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

exception BadCoercion

module type Registry = sig
  type 'a key
  type 'a data

  val register: 'a data -> unit
  val check_is_registered : 'a key -> unit
  val is_well_initialized : unit -> bool
  val get : 'a key -> 'a data
  val print : 'a key -> 'a Pp.pp

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
  val key: 'a t -> K.t

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
  module VectorH : Hashtbl_hetero.T1 with type 'a key = 'a t
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
  let key x = x

  type iter = {iter : 'a. 'a t -> unit}
  let iter f = K.iter f.iter
  type 'b fold = {fold : 'a. 'a t -> 'b -> 'b}
  let fold f acc = K.fold f.fold acc
  let hint_size = K.hint_size

  let create_key (type a) (module NT : NamedType with type t = a) : a t =
    K.create NT.name

  (** the 'a k can be used as equality witness because K gives fresh values *)
  module Eq = struct
    let eq_type :
      type a b. a t -> b t -> (a,b) Poly.iseq =
      fun a b ->
        if equal a b
        then ((Obj.magic (Poly.Eq : (a,a) Poly.eq)) : (a,b) Poly.iseq)
        else Poly.Neq

    let coerce_type :
      type a b. a t -> b t -> (a,b) Poly.eq =
      fun a b ->
        if equal a b
        then ((Obj.magic (Eq : (a,a) Poly.eq)) : (a,b) Poly.eq)
        else raise BadCoercion

    let coerce (type a) (type b) (a:a t) (b:b t) (x:a) : b =
      match coerce_type a b with
      | (Poly.Eq:(a,b) Poly.eq) -> x
  end
  module MkVector(D:sig type ('a,'b) t end) =
    Vector_hetero.Make1(struct type nonrec 'a t = 'a t end)(D)
  module MkMap(D:sig type ('a,'b) t end) =
    Intmap_hetero.Make1(struct type nonrec 'a t = 'a t end)(D)
  module Vector =
    Vector_hetero.RMake1(struct type nonrec 'a t = 'a t end)
  module VectorH : Hashtbl_hetero.T1 with type 'a key = 'a t = struct

    module VH = Vector_hetero.TMake1(struct type nonrec 'a t = 'a t end)
    type 'a key = 'a VH.key
    type t = unit VH.t
    let create = VH.create
    let size = VH.size
    let get  = VH.get
    let get_def = VH.get_def
    let set = VH.set
    let is_uninitialized = VH.is_uninitialized
    let inc_size = VH.inc_size
    type iter_initialized  = { iter : 'a. 'a -> unit; }
    type iter_initializedi = { iteri : 'a. 'a key -> 'a -> unit; }
    let iter_initializedi ({iteri}:iter_initializedi) =
      VH.iter_initializedi { VH.iteri }
    let iter_initialized ({iter}:iter_initialized) =
      VH.iter_initializedi { VH.iteri = fun _ c -> iter c }
    type 'c fold_initialized  = { fold : 'a. 'c -> 'a -> 'c; }
    type 'c fold_initializedi = { foldi : 'a. 'c -> 'a key -> 'a -> 'c; }
    let fold_initializedi ({foldi}:'c fold_initializedi) =
      VH.fold_initializedi {VH.foldi}
    let fold_initialized ({fold}:'c fold_initialized) =
      VH.fold_initializedi { foldi = fun sofar _ c -> fold sofar c }
    let copy = VH.copy
    let move = VH.move
    type printk = { printk : 'a. 'a key Format.printer }
    type printd = { printd : 'a. 'a key -> 'a Format.printer }
    let pp sep1 sep2 {printk} {printd} = VH.pp sep1 sep2 {VH.printk} {VH.printd}
    let clear _ = ()
    let remove _ = failwith "Unneeded"

  end
  module M =
    Intmap_hetero.RMake1(struct type nonrec 'a t = 'a t end)

  module Make_Registry(S:sig
      type 'a data
      val pp: 'a data -> 'a Pp.pp
      val key: 'a data -> 'a t
    end) = struct

    type 'a data = 'a S.data

    module V = MkVector(struct type ('a,'unedeed) t = 'a S.data end)

    exception UnregisteredKey : 'a t -> exn
    exception AlreadyRegisteredKey : 'a t -> exn

    let () = Exn_printer.register (fun fmt exn ->
        match exn with
        | UnregisteredKey(key) ->
          Format.fprintf fmt "The key %a have not been registered" K.pp key
        | AlreadyRegisteredKey(key) ->
          Format.fprintf fmt "The key %a have already been registered" K.pp key
        | exn -> raise exn
      )

  let registry : unit V.t = V.create 8

    let register data =
      let key = S.key data in
        V.inc_size key registry;
        assert (if not (V.is_uninitialized registry key)
                then raise (AlreadyRegisteredKey(key)) else true);
        V.set registry key data

    let check_is_registered key =
      assert (if V.is_uninitialized registry key
              then raise (UnregisteredKey(key)) else true)

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
      -> ('a1*'b1,'a2*'b2) Poly.eq option
    (** If the two arguments are physically identical then an equality witness
        between the types is returned *)

    val coerce_type : ('a1,'b1) t -> ('a2,'b2) t
      -> ('a1*'b1,'a2*'b2) Poly.eq
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
  let equal    = K.equal
  let hash  x  = K.hash x
  let key x    = x

  type iter = {iter : 'k 'd. ('k,'d) t -> unit}
  let iter f = K.iter f.iter

  let create_key (type a1) (type a2) (module NT : NamedType2 with type t = a1
                                                              and type d = a2)
    : (a1,a2) t =
    K.create NT.name

  (** the ('k,'d) k can be used as equality witness because K gives
      fresh values *)
  module Eq = struct

    let eq_type :
      type a1 b1 a2 b2. (a1,b1) t -> (a2,b2) t
      -> (a1*b1,a2*b2) Poly.eq option =
      fun a b ->
        if equal a b
        then
          let eq = (Obj.magic (Poly.Eq : (a1*b1,a1*b1) Poly.eq) : (a1*b1,a2*b2) Poly.eq)
          in Some eq
        else None

    let coerce_type :
      type a1 b1 a2 b2. (a1,b1) t -> (a2,b2) t
      -> (a1*b1,a2*b2) Poly.eq =
      fun a b ->
        if equal a b
        then
          let eq = (Obj.magic (Poly.Eq : (a1*b1,a1*b1) Poly.eq) : (a1*b1,a2*b2) Poly.eq)
          in eq
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

    exception UnregisteredKey : ('a,'b) t -> exn
    exception AlreadyRegisteredKey : ('a,'b) t -> exn

    let register data =
      let key = S.key data in
        V.inc_size key registry;
        assert (if not (V.is_uninitialized registry key)
                then raise (AlreadyRegisteredKey key) else true);
        V.set registry key data

    let check_is_registered key =
      assert (if V.is_uninitialized registry key
              then raise (UnregisteredKey key) else true)

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
