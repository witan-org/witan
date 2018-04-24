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

open Stdlib
open Containers
    
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

  exception UnregisteredKey : 'a key -> exn
  exception AlreadyRegisteredKey : 'a key -> exn

end

module type NamedType = sig
  type t
  val name : string
end

(** imperative, extensible and heterogene Arrays *)
module type S1 = sig
  type 'a key
  type ('a,'b) data
  type 'b t

  val create : int -> 'b t

  val size : 'b t -> int
  val get  : 'b t -> 'a key -> ('a,'b) data
  val get_def : 'b t -> 'a key -> ('a,'b) data -> ('a,'b) data
  val set  : 'b t -> 'a key -> ('a,'b) data -> unit

  val is_uninitialized : 'b t -> 'a key -> bool
    (** Contrary to Simple_vector it tests the size too *)
  val uninitialize     : 'b t -> 'a key -> unit

  val clear: 'b t -> unit

  val inc_size : 'a key -> 'b t -> unit

  type 'b iter_initialized = { iter: 'a. ('a,'b) data -> unit }
  val iter_initialized : 'b iter_initialized -> 'b t -> unit

  type ('b,'c) fold_initialized = { fold: 'a. 'c -> ('a,'b) data -> 'c }
  val fold_initialized :
     ('b,'c) fold_initialized -> 'c -> 'b t -> 'c

  type 'b iter_initializedi = { iteri: 'a. 'a key -> ('a,'b) data -> unit }
  val iter_initializedi :
    'b iter_initializedi -> 'b t -> unit

  type ('b,'c) fold_initializedi =
    { foldi: 'a. 'c -> 'a key -> ('a,'b) data -> 'c }
  val fold_initializedi :
     ('b,'c) fold_initializedi -> 'c -> 'b t -> 'c

  val copy : 'b t -> 'b t
  (* shallow *)

  type printk = { printk: 'a. 'a key Pp.pp }
  type 'b printd = { printd: 'a. 'a key -> ('a,'b) data Pp.pp }
  val pp:
    unit Pp.pp ->
    unit Pp.pp ->
    printk ->
    'b printd ->
    'b t Pp.pp

end

(** Same as S1 but for ('a,'b) data = 'b *)
module type R1 = sig
  include S1 with type ('a,'b) data = 'b
  val apply_initialized : ('b -> 'b) -> 'b t -> unit
end

(** Same as S1 but for ('a,'b) data = 'a *)
module type T1 = S1 with type ('a,'b) data = 'a

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
  val create_key: (module NamedType with type t = 'a) -> 'a t

  module MkVector(D:sig type ('a,'b) t end)
    : Vector_hetero.S1 with type 'a key = 'a t
                        and type ('a,'b) data = ('a,'b) D.t

  module MkMap(D:sig type ('a,'b) t end)
    : Intmap_hetero.S1 with type _ key = K.t
                        and type ('a,'b) data = ('a,'b) D.t

  module Vector  : Vector_hetero.R1 with type 'a key = 'a t
  module VectorH : Vector_hetero.T1 with type 'a key = 'a t
  module M       : Intmap_hetero.R1 with type _ key = K.t
  module Make_Registry(S:sig
      type 'a data
      val pp : 'a data -> 'a Pp.pp
      val key: 'a data -> 'a t
    end) : Registry with type 'a key := 'a t and type 'a data = 'a S.data
end

module Make_key(X:sig end) = struct
  module K = Strings.Fresh(struct end)

  type _ gadt = ..
  type 'a t = { gadt : 'a gadt;
                name : K.t;
                id   : int;
                is_eq : 'b. 'b gadt -> ('a,'b) eq option }

  let pp fmt x = K.pp fmt x.name
  let equal a b = a.id = b.id
  let compare x y = compare x.id y.id
  let hash x = x.id
  let tag    = hash
  let key x  = x.name
    
  (** the 'a k can be used as equality witness because K gives fresh values *)
  module Eq = struct
    let eq_type a b = a.is_eq b.gadt

    let coerce_type : type a b. a t -> b t -> (a,b) eq =
      fun a b -> match eq_type a b with
        | Some Eq -> Eq
        | None -> raise BadCoercion

    let coerce (type a) (type b) (a:a t) (b:b t) (x:a) : b =
      let Eq = coerce_type a b in x
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
    let is_eq : type b. b gadt -> (NT.t,b) eq option = function
      | TMP.K -> Some Eq
      | _ -> None
    in
    let key = { gadt = TMP.K;
                name = K.create NT.name;
                id = AllKeys.length all_keys;
                is_eq }
    in
    AllKeys.add all_keys (K key) ();
    key
    

  type iter = {iter : 'a. 'a t -> unit}
  let iter f = AllKeys.iter (fun (K x) () -> f.iter x) all_keys
  type 'b fold = {fold : 'a. 'a t -> 'b -> 'b}
  let fold f = AllKeys.fold (fun (K x) () -> f.fold x) all_keys

  let hint_size = K.hint_size

  module MkVector(D:sig type ('a,'b) t end)
    (* : S1 with type 'a key = 'a t
     *       and type ('a,'b) data = ('a,'b) D.t *)
  = struct
    module V = Hashtbl

    type 'a key = 'a t
    type ('a,'b) data = ('a,'b) D.t

    type 'b elt = Pair : 'a key * ('a,'b) D.t -> 'b elt
    type 'b t = (int,'b elt) V.t

    let create capacity : 'b t = V.create capacity
    let size (m: 'b t) : int = (V.stats m).V.num_bindings

    let get (type a) (m: 'b t) (k : a key) : (a, 'b) data =
      let Pair(k',r) = V.find m k.id in
      match k.is_eq k'.gadt with
      | Some Eq -> r
      | None -> raise BadCoercion
      
    let get_def : 'b t -> 'a key -> ('a, 'b) data -> ('a, 'b) data =
      fun _ -> failwith "What on earth is this?"

    let set (m: 'b t) (k: 'a key) (v : ('a, 'b) data) : unit = V.add m k.id (Pair(k,v))
      
    let is_uninitialized (m: 'b t) (k : 'a key) : bool = not(V.mem m k.id)

    let uninitialize (m: 'b t) (k : 'a key) : unit = V.remove m k.id

    let clear : 'b t -> unit = V.clear

    let inc_size : 'a key -> 'b t -> unit = fun _ -> failwith "No need for this"
      
    type 'b iter_initialized = { iter : 'a. ('a, 'b) data -> unit; }

    let iter_initialized (f : 'b iter_initialized) (m: 'b t) : unit =
      V.iter (fun _ (Pair(_,v)) -> f.iter v) m

    type ('b, 'c) fold_initialized = { fold : 'a. 'c -> ('a, 'b) data -> 'c; }

    let fold_initialized (f : ('b, 'c) fold_initialized) (seed: 'c) (m:'b t) =
      V.fold (fun _ (Pair(_,v)) sofar -> f.fold sofar v) m seed
 
    type 'b iter_initializedi = { iteri : 'a. 'a key -> ('a, 'b) data -> unit }
    let iter_initializedi (f : 'b iter_initializedi) (m: 'b t) : unit =
      V.iter (fun _ (Pair(k,v)) -> f.iteri k v) m

    type ('b, 'c) fold_initializedi = { foldi : 'a. 'c -> 'a key -> ('a, 'b) data -> 'c }
    let fold_initializedi (f: ('b, 'c) fold_initializedi) (seed : 'c) (m : 'b t) : 'c =
      V.fold (fun _ (Pair(k,v)) sofar -> f.foldi sofar k v) m seed
      
    let copy : 'b t -> 'b t = V.copy
      
    type printk = { printk : 'a. 'a key Pp.pp }
    type 'b printd = { printd : 'a. 'a key -> ('a, 'b) data Pp.pp }

    let pp (sep1 : unit Pp.pp) (sep2 : unit Pp.pp)
        (printkey : printk) (printdata : 'b printd) : 'b t Pp.pp
      =
      fun fmt t ->
        let printkeydata fmt (Pair(k,v)) =
          Format.fprintf fmt "%a%a%a" printkey.printk k sep2 () (printdata.printd k) v
        in
        let iter f m = V.iter (fun _ -> f) m in
        Pp.iter1 iter sep1 printkeydata fmt t
    
  end
  
  module Vector : Vector_hetero.R1 with type 'a key = 'a t = struct
    include MkVector(struct type (_,'b) t = 'b end)
    let iter_initialized f m = V.iter (fun _ (Pair(_,v)) -> f v) m
    let fold_initialized f seed m = V.fold (fun _ (Pair(_,v)) sofar -> f sofar v) m seed
    let apply_initialized f m =
      V.filter_map_inplace (fun _ (Pair(k,v)) -> Some(Pair(k,f v))) m
    let pp (sep1 : unit Pp.pp) (sep2 : unit Pp.pp)
        (printkey : printk) (printdata : 'b Pp.pp) : 'b t Pp.pp
      =
      fun fmt t ->
        let printkeydata fmt (Pair(k,v)) =
          Format.fprintf fmt "%a%a%a" printkey.printk k sep2 () printdata v
        in
        let iter f m = V.iter (fun _ -> f) m in
        Pp.iter1 iter sep1 printkeydata fmt t

  end
  module VectorH : Vector_hetero.T1 with type 'a key = 'a t
    = MkVector(struct type ('a,'b) t = 'a end)
  module MkMap(D:sig type ('a,'b) t end) =
    Intmap_hetero.Make1(struct type nonrec 'a t = K.t end)(D)
  module M =
    Intmap_hetero.RMake1(struct type nonrec 'a t = K.t end)

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
