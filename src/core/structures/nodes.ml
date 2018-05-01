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

exception BrokenInvariant of string
exception SolveSameRepr
exception UnwaitedEvent
exception AlreadyDead
exception AlreadyRedirected


let debug_create = Debug.register_info_flag
  ~desc:"for the core solver class creation information"
  "index"

module ThTermKind = Keys.Make_key(struct end)
module ValueKind  = Keys.Make_key(struct end)

module type ThTerm = sig
  include Stdlib.Datatype
  val key: t ThTermKind.t
end

module ThTermRegistry = ThTermKind.Make_Registry(struct
    type 'a data = (module ThTerm with type t = 'a)
    let pp (type a) ((module ThTerm): a data) = ThTerm.pp
    let key (type a) ((module ThTerm): a data) = ThTerm.key
  end)

let check_thterm_registered = ThTermRegistry.check_is_registered
let print_thterm = ThTermRegistry.print
let get_thterm = ThTermRegistry.get

module type Value = sig
  include Stdlib.Datatype
  val key: t ValueKind.t
end

module ValueRegistry = ValueKind.Make_Registry(struct
    type 'a data = (module Value with type t = 'a)
    let pp (type a) ((module V): a data) = V.pp
    let key (type a) ((module V): a data) = V.key
  end)

let check_value_registered = ValueRegistry.check_is_registered
let print_value = ValueRegistry.print
let get_value = ValueRegistry.get

module Node = struct
  type 'a r =
    | ThTerm  : int * Ty.t * 'a ThTermKind.t * 'a -> [`ThTerm] r
    | Value  : int * Ty.t * 'a ValueKind.t * 'a -> [`Value] r

  type t' = All : _ r -> t' [@@unboxed]
  type thterm = [`ThTerm] r
  type nodevalue = [`Value] r

  let tag: t' -> int = function
    | All(ThTerm(tag,_,_,_)) -> tag
    | All(Value(tag,_,_,_)) -> tag

  let names = Simple_vector.create 100
  let used_names : (* next id to use *) int DStr.H.t = DStr.H.create 100

  (** remove the empty string *)
  let () = DStr.H.add used_names "" 0

  let pp fmt x =
    Format.pp_print_char fmt '@';
    Format.pp_print_string fmt (Simple_vector.get names (tag x))

  module T = Stdlib.MakeMSH(struct
      type t = t' let tag = tag
      let pp = pp
    end)

  include T

  let next_tag, incr_tag = Util.get_counter ()

  let rename node s =
    let s = Strings.find_new_name used_names s in
    Simple_vector.set names (tag node) s

  let ty = function
    | All(ThTerm(_,ty,_,_)) -> ty
    | All(Value(_,ty,_,_)) -> ty

  module ThTermIndex = ThTermKind.MkVector
      (struct type ('a,_) t = 'a -> Ty.t -> thterm end)

  let semindex : unit ThTermIndex.t = ThTermIndex.create 8

  let thterm sem v ty : thterm =
    ThTermRegistry.check_is_registered sem;
    ThTermIndex.get semindex sem v ty

  module ValueIndex = ValueKind.MkVector
      (struct type ('a,'unedeed) t = 'a -> Ty.t -> nodevalue end)

  let valueindex : unit ValueIndex.t = ValueIndex.create 8

  let nodevalue value v ty : nodevalue =
    ValueRegistry.check_is_registered value;
    ValueIndex.get valueindex value v ty

  let of_thterm : thterm -> t = fun t -> All t
  let of_nodevalue : nodevalue -> t = fun t -> All t

  let index_sem   sem v ty = of_thterm (thterm sem v ty)
  let index_value sem v ty = of_nodevalue (nodevalue sem v ty)

end

module ThTerm = struct
  include Stdlib.MakeMSH(struct
      type t = Node.thterm
      let tag: t -> int = function
        | Node.ThTerm(tag,_,_,_) -> tag
      let pp fmt : t -> unit = function
        | Node.ThTerm(_,_,sem,v) -> print_thterm sem fmt v
    end)

  let index = Node.thterm
  let node = Node.of_thterm
  let ty : t -> Ty.t = function
    | Node.ThTerm(_,ty,_,_) -> ty


end

module type RegisteredThTerm = sig
  type s
  val key: s ThTermKind.t
  (** thterm *)
  include Datatype

  val index: s -> Ty.t -> t
  (** Return a thterm from a semantical value *)

  val node: t -> Node.t
  (** Return a node from a thterm *)

  val ty: t -> Ty.t
  (** Return the type from a thterm *)

  val sem: t -> s
  (** Return the sem from a thterm *)

  val thterm: t -> ThTerm.t
  val of_thterm: ThTerm.t -> t option

  val coerce_thterm: ThTerm.t -> t

end



module RegisterThTerm (D:ThTerm) : RegisteredThTerm with type s = D.t = struct

  module HC = Hashcons.MakeTag(struct
      open Node
      type t = thterm

      let next_tag = Node.next_tag
      let incr_tag = Node.incr_tag

      let equal: t -> t -> bool = fun a b ->
        match a, b with
        | ThTerm(_,tya,sema,va), ThTerm(_,tyb,semb,vb) ->
          match ThTermKind.Eq.coerce_type sema D.key,
                ThTermKind.Eq.coerce_type semb D.key with
          | Poly.Eq, Poly.Eq -> Ty.equal tya tyb && D.equal va vb

      let hash: t -> int = fun a ->
        match a with
        | ThTerm(_,tya,sema,va) ->
          let Poly.Eq = ThTermKind.Eq.coerce_type sema D.key in
          Hashcons.combine (Ty.hash tya) (D.hash va)

      let set_tag: int -> t -> t =
        fun tag (ThTerm(_,ty,sem,v)) -> ThTerm(tag,ty,sem,v)

      let tag: t -> int = fun (ThTerm(tag,_,_,_)) -> tag

      let pp fmt x =
        Format.pp_print_char fmt '@';
        Format.pp_print_string fmt (Simple_vector.get names (tag x))
    end)

  include HC

  type s = D.t
  let key = D.key

  let tag: t -> int = function
    | Node.ThTerm(tag,_,_,_) -> tag

  let index v ty =
    let node =
      HC.hashcons3
        (fun tag sem v ty -> Node.ThTerm(tag,ty,sem,v))
        D.key v ty in
    let i = tag node in
    Simple_vector.inc_size (i+1) Node.names;
    begin
      if Simple_vector.is_uninitialized Node.names i then
        let s = Strings.find_new_name Node.used_names ""
        (** TODO use ThTerm.pp or Sem.print_debug *) in
        Debug.dprintf3 debug_create "[Egraph] @[@@%s is %a@]"
          s D.pp v;
        Simple_vector.set Node.names i s
    end;
    node

  let node = Node.of_thterm

  let sem : t -> D.t =
    fun (Node.ThTerm(_,_,sem,v)) ->
      let Poly.Eq = ThTermKind.Eq.coerce_type sem D.key in v

  let ty = ThTerm.ty

  let thterm: t -> ThTerm.t = fun x -> x

  let of_thterm: ThTerm.t -> t option = function
    | Node.ThTerm(_,_,sem',_) as v when ThTermKind.equal sem' D.key -> Some v
    | _ -> None

  let coerce_thterm: ThTerm.t -> t =
    fun (Node.ThTerm(_,_,sem',_) as v) -> assert (ThTermKind.equal sem' D.key); v

  let () =
    ThTermRegistry.register (module D: ThTerm with type t = D.t);
    Node.ThTermIndex.set Node.semindex D.key index

end

module Value = struct
  include Stdlib.MakeMSH(struct
      type t = Node.nodevalue
      let tag: t -> int = function
        | Node.Value(tag,_,_,_) -> tag
      let pp fmt : t -> unit = function
        | Node.Value(_,_,value,v) -> print_value value fmt v
    end)

  let index = Node.nodevalue
  let node = Node.of_nodevalue
  let ty : t -> Ty.t = function
    | Node.Value(_,ty,_,_) -> ty

  let value : type a. a ValueKind.t -> t -> a option =
    fun value (Node.Value(_,_,value',v)) ->
      match ValueKind.Eq.eq_type value value' with
      | Poly.Neq -> None
      | Poly.Eq  -> Some v

  let semantic_equal (x:t) (y:t) : [ `Uncomparable | `Equal | `Disequal ] =
    match x, y with
    | Node.Value(_,_,xk,_), Node.Value(_,_,yk,_) when not (ValueKind.equal xk yk) ->
      `Uncomparable
    | _ -> if equal x y then `Equal else `Disequal

end

module type RegisteredValue = sig
  type s
  module V : Value with type t = s
  val key: s ValueKind.t
  (** nodevalue *)
  include Datatype

  val index: ?basename:string -> s -> Ty.t -> t
  (** Return a nodevalue from a valueantical value *)

  val node: t -> Node.t
  (** Return a class from a nodevalue *)

  val ty: t -> Ty.t
  (** Return the type from a nodevalue *)

  val value: t -> s
  (** Return the value from a nodevalue *)

  val nodevalue: t -> Value.t
  val of_nodevalue: Value.t -> t option

  val coerce_nodevalue: Value.t -> t

end


module RegisteredValueRegistry = ValueKind.Make_Registry(struct
    type 'a data = (module RegisteredValue with type s = 'a)
    let pp (type a) (value: a data) =
      let module RegisteredValue = (val value) in
      RegisteredValue.V.pp
    let key (type a) (value: a data) =
      let module RegisteredValue = (val value) in
      RegisteredValue.key
  end)

let get_registered_value = RegisteredValueRegistry.get


module RegisterValue (D:Value) : RegisteredValue with type s = D.t = struct

  module All = struct

  module V = D
  module HC = Hashcons.MakeTag(struct
      open Node
      type t = nodevalue

      let next_tag = Node.next_tag
      let incr_tag = Node.incr_tag

      let equal: t -> t -> bool = fun a b ->
        match a, b with
        | Value(_,tya,valuea,va), Value(_,tyb,valueb,vb) ->
          match ValueKind.Eq.coerce_type valuea D.key,
                ValueKind.Eq.coerce_type valueb D.key with
          | Poly.Eq, Poly.Eq  -> Ty.equal tya tyb && D.equal va vb

      let hash: t -> int = fun a ->
        match a with
        | Value(_,tya,valuea,va) ->
          let Poly.Eq = ValueKind.Eq.coerce_type valuea D.key in
          Hashcons.combine (Ty.hash tya) (D.hash va)

      let set_tag: int -> t -> t = fun tag x ->
        match x with
        | Value(_,ty,value,v) -> Value(tag,ty,value,v)

      let tag: t -> int = function
        | Value(tag,_,_,_) -> tag

      let pp fmt x =
        Format.pp_print_char fmt '@';
        Format.pp_print_string fmt (Simple_vector.get names (tag x))
    end)

  include HC

  type s = D.t
  let key = D.key

  let tag: t -> int = function
    | Node.Value(tag,_,_,_) -> tag

  let index ?(basename="") v ty =
    let node =
      HC.hashcons3
        (fun tag value v ty -> Node.Value(tag,ty,value,v))
        D.key v ty in
    let i = tag node in
    Simple_vector.inc_size (i+1) Node.names;
    begin
      if Simple_vector.is_uninitialized Node.names i then
        let s = Strings.find_new_name Node.used_names basename in
        Debug.dprintf3 debug_create "[Egraph] @[@@%s is %a@]"
          s D.pp v;
        Simple_vector.set Node.names i s
    end;
    node

  let node = Node.of_nodevalue

  let value : t -> D.t = function
    | Node.Value(_,_,value,v) ->
      let Poly.Eq = ValueKind.Eq.coerce_type value D.key in v

  let ty = Value.ty

  let nodevalue: t -> Value.t = fun x -> x

  let of_nodevalue: Value.t -> t option = function
    | Node.Value(_,_,value',_) as v when ValueKind.equal value' D.key -> Some v
    | _ -> None

  let coerce_nodevalue: Value.t -> t = function
    | Node.Value(_,_,value',_) as v -> assert (ValueKind.equal value' D.key); v

  end

  include All
  let () =
    ValueRegistry.register (module D: Value with type t = D.t);
    RegisteredValueRegistry.register (module All: RegisteredValue with type s = D.t);
    Node.ValueIndex.set Node.valueindex D.key (fun v ty -> index v ty)

end

module Only_for_solver = struct
  type sem_of_node =
    | ThTerm: 'a ThTermKind.t * 'a  -> sem_of_node

  let thterm: Node.t -> ThTerm.t option = function
    | Node.All Node.Value _ -> None
    | Node.All (Node.ThTerm _ as x) -> Some x

  let sem_of_node: ThTerm.t -> sem_of_node = function
    | Node.ThTerm (_,_,sem,v) -> ThTerm(sem,v)

  type value_of_node =
    | Value: 'a ValueKind.t * 'a  -> value_of_node

  let nodevalue: Node.t -> Value.t option = function
    | Node.All Node.ThTerm _ -> None
    | Node.All (Node.Value _ as x) -> Some x

  let value_of_node: Value.t -> value_of_node = function
    | Node.Value (_,_,value,v) -> Value(value,v)

  let node_of_thterm : ThTerm.t -> Node.t = ThTerm.node
  let node_of_nodevalue : Value.t -> Node.t = Value.node

  type opened_node =
    | ThTerm : ThTerm.t -> opened_node
    | Value  : Value.t -> opened_node

  let open_node : Node.t -> opened_node = function
    | Node.All (Node.ThTerm _ as x) -> ThTerm x
    | Node.All (Node.Value _ as x) -> Value x

  let is_value : Node.t -> bool = function
    | Node.All(Node.ThTerm _) -> false
    | Node.All(Node.Value _) -> true

end


let check_initialization () =
  ThTermRegistry.is_well_initialized () &&
  ValueRegistry.is_well_initialized ()
