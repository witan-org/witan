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

open Std

module Strings = Witan_popop_lib.Strings
module Exn_printer = Witan_popop_lib.Exn_printer

include Keys_sig

module Make_key(X:sig end) = struct
  module K = Strings.Fresh(struct end)

  type _ gadt = ..
  type 'a t = { gadt : 'a gadt;
                name : K.t;
                id   : int;
                iseq : 'b. 'b gadt -> ('a,'b) Poly.iseq }

  let pp fmt x = K.pp fmt x.name
  let equal a b = a.id = b.id
  let compare x y = compare x.id y.id
  let hash x = x.id
  let tag  x = x.id
  let key  x = x.name
                 
  (** the 'a k can be used as equality witness because K gives fresh values *)
  module Eq = struct
    let eq_type a b = a.iseq b.gadt
    let coerce_type a b = eq_type a b |> Poly.eq
    let coerce (type a) (type b) (a:a t) (b:b t) (x:a) : b =
      let Poly.Eq = coerce_type a b in x
  end

  type key = K : _ t -> key [@@unboxed]

  module AllKeys = Hashtbl.Make(struct
      type t = key
      let equal (K a) (K b) = equal a b
      let hash (K a) = a.id
    end)

  let all_keys = AllKeys.create 17

  let create_key (type a) (module NT : NamedType with type t = a) : a t =
    let module TMP = struct
      type _ gadt += K : NT.t gadt
    end in
    let iseq : type b. b gadt -> (NT.t,b) Poly.iseq = function
      | TMP.K -> Poly.Eq
      | _ -> Poly.Neq
    in
    let key = { gadt = TMP.K;
                name = K.create NT.name;
                id = AllKeys.length all_keys;
                iseq }
    in
    AllKeys.add all_keys (K key) ();
    key


  type iter = {iter : 'a. 'a t -> unit}
  let iter f = AllKeys.iter (fun (K x) () -> f.iter x) all_keys
  type 'b fold = {fold : 'a. 'a t -> 'b -> 'b}
  let fold f = AllKeys.fold (fun (K x) () -> f.fold x) all_keys

  let hint_size = K.hint_size

  module K1 = struct
    type nonrec 'a t = 'a t
    let equal = Eq.eq_type
    let tag = tag
  end

  module MkVector(D:sig type ('a,'b) t end) = Hashtbl_hetero.MakeS1(K1)(D)
  module Vector  = Hashtbl_hetero.MakeR1(K1)
  module VectorH = Hashtbl_hetero.MakeT1(K1)
  module MkMap(D:sig type ('a,'b) t end) = Map_hetero.MakeS(K1)(D)
  module M = Map_hetero.MakeR(K1)

  module Make_Registry(S:sig
      type 'a data
      val pp: 'a data -> 'a Format.printer
      val key: 'a data -> 'a t
    end) = struct

    type 'a data = 'a S.data

    module V = MkVector(struct type ('a,'unedeed) t = 'a S.data end)

    exception UnregisteredKey : 'a t -> exn
    exception AlreadyRegisteredKey : 'a t -> exn

    let () = Exn_printer.register (fun fmt exn ->
        match exn with
        | UnregisteredKey(key) ->
          Format.fprintf fmt "The key %a have not been registered" pp key
        | AlreadyRegisteredKey(key) ->
          Format.fprintf fmt "The key %a have already been registered" pp key
        | exn -> raise exn
      )

    let registry : unit V.t = V.create 8

    let register data =
      let key = S.key data in
      (* V.inc_size key.name registry; *)
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

    let print (type a) (k : a t) fmt s =
      let data = get k in
      S.pp data fmt s
  end

end





module Make_key2(X:sig end) : Key2 = struct
  module K = Strings.Fresh(struct end)

  type (_,_) gadt = ..
  type ('k,'d) t = { gadt : ('k,'d) gadt;
                     name : K.t;
                     id   : int;
                     iseq : 'b1 'b2. ('b1,'b2) gadt -> ('k*'d,'b1*'b2) Poly.iseq }

  let pp fmt x = K.pp fmt x.name
  let equal a b = a.id = b.id
  let compare x y = compare x.id y.id
  let hash x = x.id
  let tag    = hash
  let key x = x.name
  
  (** the ('k,'d) k can be used as equality witness because K gives fresh values *)
  module Eq = struct
    let eq_type a b = a.iseq b.gadt
    let coerce_type a b = eq_type a b |> Poly.eq
  end

  type key = K : _ t -> key [@@unboxed]

  module AllKeys = Hashtbl.Make(struct
      type t = key
      let equal (K a) (K b) = equal a b
      let hash (K a) = a.id
    end)

  let all_keys = AllKeys.create 17

  let create_key (type a1) (type a2) (module NT : NamedType2 with type t = a1
                                                              and type d = a2)
    : (a1,a2) t =
    let module TMP = struct
      type (_,_) gadt += K : (NT.t,NT.d) gadt
    end in
    let iseq : type b1 b2. (b1,b2) gadt -> (NT.t*NT.d,b1*b2) Poly.iseq = function
      | TMP.K -> Poly.Eq
      | _ -> Poly.Neq
    in
    let key = { gadt = TMP.K;
                name = K.create NT.name;
                id = AllKeys.length all_keys;
                iseq }
    in
    AllKeys.add all_keys (K key) ();
    key

  type iter = {iter : 'k 'd. ('k,'d) t -> unit}
  let iter f = AllKeys.iter (fun (K x) () -> f.iter x) all_keys
  type 'b fold = {fold : 'a1 'a2. ('a1,'a2) t -> 'b -> 'b}
  let fold f = AllKeys.fold (fun (K x) () -> f.fold x) all_keys

  module K2 = struct
    type nonrec ('a1,'a2) t = ('a1,'a2) t
    let equal = Eq.eq_type
    let tag = tag
  end

  module MkVector(D:sig type ('k,'d,'b) t end) = Hashtbl_hetero.Make2(K2)(D)

  module Make_Registry(S:sig
      type ('k,'d) data
      val ppk: ('k,'d) data -> 'k Format.printer
      val ppd: ('k,'d) data -> 'd Format.printer
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
