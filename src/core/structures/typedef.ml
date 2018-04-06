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

exception BrokenInvariant of string
exception SolveSameRepr
exception UnwaitedEvent
exception AlreadyDead
exception AlreadyRedirected


let debug_create = Debug.register_info_flag
  ~desc:"for the core solver class creation information"
  "index"

module Sem = Keys.Make_key(struct end)
module Value = Keys.Make_key(struct end)

(** sem *)
type 'a sem = 'a Sem.t

module type Sem = sig
  include Stdlib.Datatype
  val key: t sem
end

module SemRegistry = Sem.Make_Registry(struct
    type 'a data = (module Sem with type t = 'a)
    let pp (type a) (sem: a data) =
      let module Sem = (val sem) in
      Sem.pp
    let key (type a) (sem: a data) =
      let module Sem = (val sem) in
      Sem.key
  end)

let check_sem_registered = SemRegistry.check_is_registered
let print_sem = SemRegistry.print
let get_sem = SemRegistry.get

(** value *)
type 'a value = 'a Value.t

module type Value = sig
  include Stdlib.Datatype
  val key: t value
end

module ValueRegistry = Value.Make_Registry(struct
    type 'a data = (module Value with type t = 'a)
    let pp (type a) (value: a data) =
      let module Value = (val value) in
      Value.pp
    let key (type a) (value: a data) =
      let module Value = (val value) in
      Value.key
  end)

let check_value_registered = ValueRegistry.check_is_registered
let print_value = ValueRegistry.print
let get_value = ValueRegistry.get

module Node = struct
  type 'a r =
    | Sem  : int * Ty.t * 'a sem * 'a -> [>`Sem] r
    | Value  : int * Ty.t * 'a value * 'a -> [>`Value] r

  type t' = [ `Sem | `Value] r
  type thterm = [`Sem] r
  type nodevalue = [`Value] r

  let tag: t' -> int = function
    | Sem(tag,_,_,_) -> tag
    | Value(tag,_,_,_) -> tag

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
    | Sem(_,ty,_,_) -> ty
    | Value(_,ty,_,_) -> ty

  module SemIndex = Sem.MkVector
      (struct type ('a,'unedeed) t = 'a -> Ty.t -> thterm end)

  let semindex : unit SemIndex.t = SemIndex.create 8

  let thterm sem v ty : thterm =
    SemRegistry.check_is_registered sem;
    (SemIndex.get semindex sem) v ty

  module ValueIndex = Value.MkVector
      (struct type ('a,'unedeed) t = 'a -> Ty.t -> nodevalue end)

  let valueindex : unit ValueIndex.t = ValueIndex.create 8

  let nodevalue value v ty : nodevalue =
    ValueRegistry.check_is_registered value;
    (ValueIndex.get valueindex value) v ty

  (** Just used for checking the typability *)
  let _of_thterm : thterm -> t = function
    | Sem(tag,ty,sem,v) -> Sem(tag,ty,sem,v)

  (** IF the previous function is typable this one is correct:
      I'm not able to defined is without obj.magic
  *)
  let of_thterm : thterm -> t = Obj.magic

  (** Just used for checking the typability *)
  let _of_nodevalue : nodevalue -> t = function
    | Value(tag,ty,value,v) -> Value(tag,ty,value,v)

  (** IF the previous function is typable this one is correct:
      I'm not able to defined is without obj.magic
  *)
  let of_nodevalue : nodevalue -> t = Obj.magic

  let index_sem   sem v ty = of_thterm (thterm sem v ty)
  let index_value sem v ty = of_nodevalue (nodevalue sem v ty)

end

module ThTerm = struct
  include Stdlib.MakeMSH(struct
      type t = Node.thterm
      let tag: t -> int = function
        | Node.Sem(tag,_,_,_) -> tag
      let pp fmt : t -> unit = function
        | Node.Sem(_,_,sem,v) -> print_sem sem fmt v
    end)

  let index = Node.thterm
  let node = Node.of_thterm
  let ty : t -> Ty.t = function
    | Node.Sem(_,ty,_,_) -> ty


end

module type RegisteredSem = sig
  type s
  val key: s sem
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



module RegisterSem (D:Sem) : RegisteredSem with type s = D.t = struct

  module HC = Hashcons.MakeTag(struct
      open Node
      type t = thterm

      let next_tag = Node.next_tag
      let incr_tag = Node.incr_tag

      let equal: t -> t -> bool = fun a b ->
        match a, b with
        | Sem(_,tya,sema,va), Sem(_,tyb,semb,vb) ->
          match Sem.Eq.coerce_type sema D.key,
                Sem.Eq.coerce_type semb D.key with
          | Keys.Eq, Keys.Eq  ->
             Ty.equal tya tyb && D.equal va vb

      let hash: t -> int = fun a ->
        match a with
        | Sem(_,tya,sema,va) ->
          match Sem.Eq.coerce_type sema D.key with
          | Keys.Eq ->
            Hashcons.combine (Ty.hash tya) (D.hash va)

      let set_tag: int -> t -> t = fun tag x ->
        match x with
        | Sem(_,ty,sem,v) -> Sem(tag,ty,sem,v)

      let tag: t -> int = function
        | Sem(tag,_,_,_) -> tag

      let pp fmt x =
        Format.pp_print_char fmt '@';
        Format.pp_print_string fmt (Simple_vector.get names (tag x))
    end)

  include HC

  type s = D.t
  let key = D.key

  let tag: t -> int = function
    | Node.Sem(tag,_,_,_) -> tag

  let index v ty =
    let node =
      HC.hashcons3
        (fun tag sem v ty -> Node.Sem(tag,ty,sem,v))
        D.key v ty in
    let i = tag node in
    Simple_vector.inc_size (i+1) Node.names;
    begin
      if Simple_vector.is_uninitialized Node.names i then
        let s = Strings.find_new_name Node.used_names ""
        (** TODO use Sem.pp or Sem.print_debug *) in
        Debug.dprintf3 debug_create "[Egraph] @[@@%s is %a@]"
          s D.pp v;
        Simple_vector.set Node.names i s
    end;
    node

  let node = Node.of_thterm

  let sem : t -> D.t = function
    | Node.Sem(_,_,sem,v) ->
      match Sem.Eq.coerce_type sem D.key with
      | Keys.Eq -> v

  let ty = ThTerm.ty

  let thterm: t -> ThTerm.t = fun x -> x

  let of_thterm: ThTerm.t -> t option = function
    | Node.Sem(_,_,sem',_) as v when Sem.equal sem' D.key -> Some v
    | _ -> None

  let coerce_thterm: ThTerm.t -> t = function
    | Node.Sem(_,_,sem',_) as v -> assert (Sem.equal sem' D.key); v

  let () =
    SemRegistry.register (module D: Sem with type t = D.t);
    Node.SemIndex.set Node.semindex D.key (fun v ty -> index v ty)

end

module Values = struct
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

  let value : type a. a Value.t -> t -> a option = fun value t ->
    match t with
    | Node.Value(_,_,value',v) ->
      match Value.Eq.eq_type value value' with
      | None -> None
      | Some Keys.Eq -> Some v

end

module type RegisteredValue = sig
  type s
  module V : Value with type t = s
  val key: s value
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

  val nodevalue: t -> Values.t
  val of_nodevalue: Values.t -> t option

  val coerce_nodevalue: Values.t -> t

end


module RegisteredValueRegistry = Value.Make_Registry(struct
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
          match Value.Eq.coerce_type valuea D.key,
                Value.Eq.coerce_type valueb D.key with
          | Keys.Eq, Keys.Eq  ->
             Ty.equal tya tyb && D.equal va vb

      let hash: t -> int = fun a ->
        match a with
        | Value(_,tya,valuea,va) ->
          match Value.Eq.coerce_type valuea D.key with
          | Keys.Eq ->
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
      match Value.Eq.coerce_type value D.key with
      | Keys.Eq -> v

  let ty = Values.ty

  let nodevalue: t -> Values.t = fun x -> x

  let of_nodevalue: Values.t -> t option = function
    | Node.Value(_,_,value',_) as v when Value.equal value' D.key -> Some v
    | _ -> None

  let coerce_nodevalue: Values.t -> t = function
    | Node.Value(_,_,value',_) as v -> assert (Value.equal value' D.key); v

  end

  include All
  let () =
    ValueRegistry.register (module D: Value with type t = D.t);
    RegisteredValueRegistry.register (module All: RegisteredValue with type s = D.t);
    Node.ValueIndex.set Node.valueindex D.key (fun v ty -> index v ty)

end

module Only_for_solver = struct
  type sem_of_node =
    | Sem: 'a sem * 'a  -> sem_of_node

  let thterm: Node.t -> ThTerm.t option = function
    | Node.Value _ -> None
    | Node.Sem _ as x -> Some (Obj.magic x: ThTerm.t)

  let sem_of_node: ThTerm.t -> sem_of_node = function
    | Node.Sem (_,_,sem,v) -> Sem(sem,v)

  type value_of_node =
    | Value: 'a value * 'a  -> value_of_node

  let nodevalue: Node.t -> Values.t option = function
    | Node.Sem _ -> None
    | Node.Value _ as x -> Some (Obj.magic x: Values.t)

  let value_of_node: Values.t -> value_of_node = function
    | Node.Value (_,_,value,v) -> Value(value,v)

  let node_of_thterm : ThTerm.t -> Node.t = ThTerm.node
  let node_of_nodevalue : Values.t -> Node.t = Values.node

  type opened_node =
    | Sem  : ThTerm.t -> opened_node
    | Value  : Values.t -> opened_node

  let open_node : Node.t -> opened_node = function
    | Node.Sem _ as x -> Sem (Obj.magic x: ThTerm.t)
    | Node.Value _ as x -> Value (Obj.magic x: Values.t)
end


let check_initialization () =
  SemRegistry.is_well_initialized () &&
  ValueRegistry.is_well_initialized ()
