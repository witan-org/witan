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

exception BrokenInvariant of string
exception SolveSameRepr
exception UnregisteredKey
exception AlreadyRegisteredKey
exception UnwaitedEvent
exception AlreadyDead
exception AlreadyRedirected


let debug_create = Debug.register_info_flag
  ~desc:"for the core solver class creation information"
  "index"

module Ty = struct
  module Constr= Strings.Fresh(struct end)

  type ty = { ctr: Constr.t; args: ty IArray.t; tag: int}

  module Ty = Hashcons.Make(struct
      type t = ty

      let equal ty1 ty2 =
        Constr.equal ty1.ctr ty2.ctr &&
        IArray.equal (fun x1 x2 -> DInt.equal x1.tag x2.tag) ty1.args ty2.args

      let hash {ctr;args} =
          Hashcons.combine (Constr.hash ctr)
            (IArray.hash (fun x1 -> x1.tag) args)

      let set_tag i t = {t with tag = i}
      let tag t = t.tag

      let rec pp fmt = function
        | {ctr;args} when IArray.length args = 0 -> Constr.pp fmt ctr
        | {ctr;args} -> Format.fprintf fmt "%a(%a)"
                          Constr.pp ctr (IArray.pp Pp.comma pp) args
    end)

  let app ctr args = Ty.hashcons {ctr;args; tag = -1}
  let args0 = IArray.of_array [||]
  let ctr ctr = app ctr args0

  include Ty

end

module Dom = Keys.Make_key(struct end)
module Sem = Keys.Make_key(struct end)
module Value = Keys.Make_key(struct end)

type 'a dom = 'a Dom.t

(** sem *)
type 'a sem = 'a Sem.t

module type Sem = sig
  include Stdlib.Datatype
  val key: t sem
end

module VSem = Sem.MkVector
  (struct type ('a,'unedeed) t =
            (module Sem with type t = 'a)
   end)

let defined_sem : unit VSem.t = VSem.create 8
let sem_uninitialized sem = VSem.is_uninitialized defined_sem sem
let get_sem k =
  assert (if sem_uninitialized k then raise UnregisteredKey else true);
  VSem.get defined_sem k

let print_sem (type a) (k : a sem) fmt s =
  let sem = get_sem k in
  let module S = (val sem : Sem with type t = a) in
  S.pp fmt s

(** value *)
type 'a value = 'a Value.t

module type Value = sig
  include Stdlib.Datatype
  val key: t value
end

module VValue = Value.MkVector
  (struct type ('a,'unedeed) t =
            (module Value with type t = 'a)
   end)

let defined_value : unit VValue.t = VValue.create 8
let value_uninitialized value = VValue.is_uninitialized defined_value value
let get_value k =
  assert (if value_uninitialized k then raise UnregisteredKey else true);
  VValue.get defined_value k

let print_value (type a) (k : a value) fmt s =
  let value = get_value k in
  let module S = (val value : Value with type t = a) in
  S.pp fmt s

(** Dem *)

module Dem = Keys.Make_key2(struct end)

type ('k,'d) dem = ('k,'d) Dem.t


module Cl = struct
  type 'a r =
    | Fresh: int * Ty.t -> [>`Fresh] r
    | Fresh_to_reg: int * Ty.t * ('event,'r) dem * 'event -> [>`Fresh] r
    | Sem  : int * Ty.t * 'a sem * 'a -> [>`Sem] r
    | Value  : int * Ty.t * 'a value * 'a -> [>`Value] r

  type t' = [ `Fresh | `Sem | `Value] r
  type clsem = [`Sem] r
  type clvalue = [`Value] r

  let tag: t' -> int = function
    | Fresh(tag,_) -> tag
    | Fresh_to_reg(tag,_,_,_) -> tag
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

  let fresh ?to_reg s ty : t =
    let i = next_tag () in
    incr_tag ();
    let s = Strings.find_new_name used_names s in
    Simple_vector.inc_size (i+1) names;
    Simple_vector.set names i s;
    match to_reg with
    | None ->
      Debug.dprintf1 debug_create "[Solver] @[fresh @@%s@]" s;
      Fresh(i,ty)
    | Some (dem,event) ->
      Debug.dprintf1 debug_create "[Solver] @[fresh to reg @@%s@]" s;
      Fresh_to_reg(i,ty,dem,event)

  let rename cl s =
    let s = Strings.find_new_name used_names s in
    Simple_vector.set names (tag cl) s

  let ty = function | Fresh (_,ty)
                    | Fresh_to_reg (_,ty,_,_)
                    | Sem(_,ty,_,_) -> ty
                    | Value(_,ty,_,_) -> ty

  module SemIndex = Sem.MkVector
      (struct type ('a,'unedeed) t = 'a -> Ty.t -> clsem end)

  let semindex : unit SemIndex.t = SemIndex.create 8

  let clsem sem v ty : clsem =
    assert (if sem_uninitialized sem then raise UnregisteredKey else true);
    (SemIndex.get semindex sem) v ty

  module ValueIndex = Value.MkVector
      (struct type ('a,'unedeed) t = 'a -> Ty.t -> clvalue end)

  let valueindex : unit ValueIndex.t = ValueIndex.create 8

  let clvalue value v ty : clvalue =
    assert (if value_uninitialized value then raise UnregisteredKey else true);
    (ValueIndex.get valueindex value) v ty

  (** Just used for checking the typability *)
  let _of_clsem : clsem -> t = function
    | Sem(tag,ty,sem,v) -> Sem(tag,ty,sem,v)

  (** IF the previous function is typable this one is correct:
      I'm not able to defined is without obj.magic
  *)
  let of_clsem : clsem -> t = Obj.magic

  (** Just used for checking the typability *)
  let _of_clvalue : clvalue -> t = function
    | Value(tag,ty,value,v) -> Value(tag,ty,value,v)

  (** IF the previous function is typable this one is correct:
      I'm not able to defined is without obj.magic
  *)
  let of_clvalue : clvalue -> t = Obj.magic

  let index sem v ty = of_clsem (clsem sem v ty)

end

module ClSem = struct
  include Stdlib.MakeMSH(struct
      type t = Cl.clsem
      let tag: t -> int = function
        | Cl.Sem(tag,_,_,_) -> tag
      let pp fmt : t -> unit = function
        | Cl.Sem(_,_,sem,v) -> print_sem sem fmt v
    end)

  let index = Cl.clsem
  let cl = Cl.of_clsem
  let ty : t -> Ty.t = function
    | Cl.Sem(_,ty,_,_) -> ty


end

module type RegisteredSem = sig
  type s
  val key: s sem
  (** clsem *)
  include Datatype

  val index: s -> Ty.t -> t
  (** Return a clsem from a semantical value *)

  val cl: t -> Cl.t
  (** Return a class from a clsem *)

  val ty: t -> Ty.t
  (** Return the type from a clsem *)

  val sem: t -> s
  (** Return the sem from a clsem *)

  val clsem: t -> ClSem.t
  val of_clsem: ClSem.t -> t option

  val coerce_clsem: ClSem.t -> t

end



module RegisterSem (D:Sem) : RegisteredSem with type s = D.t = struct

  module HC = Hashcons.MakeTag(struct
      open Cl
      type t = clsem

      let next_tag = Cl.next_tag
      let incr_tag = Cl.incr_tag

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
    | Cl.Sem(tag,_,_,_) -> tag

  let index v ty =
    let cl =
      HC.hashcons3
        (fun tag sem v ty -> Cl.Sem(tag,ty,sem,v))
        D.key v ty in
    let i = tag cl in
    Simple_vector.inc_size (i+1) Cl.names;
    begin
      if Simple_vector.is_uninitialized Cl.names i then
        let s = Strings.find_new_name Cl.used_names ""
        (** TODO use Sem.pp or Sem.print_debug *) in
        Debug.dprintf3 debug_create "[Solver] @[index %a into @@%s@]"
          D.pp v s;
        Simple_vector.set Cl.names i s
    end;
    cl

  let cl = Cl.of_clsem

  let sem : t -> D.t = function
    | Cl.Sem(_,_,sem,v) ->
      match Sem.Eq.coerce_type sem D.key with
      | Keys.Eq -> v

  let ty = ClSem.ty

  let clsem: t -> ClSem.t = fun x -> x

  let of_clsem: ClSem.t -> t option = function
    | Cl.Sem(_,_,sem',_) as v when Sem.equal sem' D.key -> Some v
    | _ -> None

  let coerce_clsem: ClSem.t -> t = function
    | Cl.Sem(_,_,sem',_) as v -> assert (Sem.equal sem' D.key); v

  let () =
    VSem.inc_size D.key defined_sem;
    assert (if not (VSem.is_uninitialized defined_sem D.key)
      then raise AlreadyRegisteredKey else true);
    let sem = (module D: Sem with type t = D.t) in
    VSem.set defined_sem D.key sem;
    Cl.SemIndex.set Cl.semindex D.key (fun v ty -> index v ty)

end

module ClValue = struct
  include Stdlib.MakeMSH(struct
      type t = Cl.clvalue
      let tag: t -> int = function
        | Cl.Value(tag,_,_,_) -> tag
      let pp fmt : t -> unit = function
        | Cl.Value(_,_,value,v) -> print_value value fmt v
    end)

  let index = Cl.clvalue
  let cl = Cl.of_clvalue
  let ty : t -> Ty.t = function
    | Cl.Value(_,ty,_,_) -> ty


end

module type RegisteredValue = sig
  type s
  val key: s value
  (** clvalue *)
  include Datatype

  val index: s -> Ty.t -> t
  (** Return a clvalue from a valueantical value *)

  val cl: t -> Cl.t
  (** Return a class from a clvalue *)

  val ty: t -> Ty.t
  (** Return the type from a clvalue *)

  val value: t -> s
  (** Return the value from a clvalue *)

  val clvalue: t -> ClValue.t
  val of_clvalue: ClValue.t -> t option

  val coerce_clvalue: ClValue.t -> t

end

module RegisterValue (D:Value) : RegisteredValue with type s = D.t = struct

  module HC = Hashcons.MakeTag(struct
      open Cl
      type t = clvalue

      let next_tag = Cl.next_tag
      let incr_tag = Cl.incr_tag

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
    | Cl.Value(tag,_,_,_) -> tag

  let index v ty =
    let cl =
      HC.hashcons3
        (fun tag value v ty -> Cl.Value(tag,ty,value,v))
        D.key v ty in
    let i = tag cl in
    Simple_vector.inc_size (i+1) Cl.names;
    begin
      if Simple_vector.is_uninitialized Cl.names i then
        let s = Strings.find_new_name Cl.used_names ""
        (** TODO use Value.pp or Value.print_debug *) in
        Debug.dprintf3 debug_create "[Solver] @[index %a into @@%s@]"
          D.pp v s;
        Simple_vector.set Cl.names i s
    end;
    cl

  let cl = Cl.of_clvalue

  let value : t -> D.t = function
    | Cl.Value(_,_,value,v) ->
      match Value.Eq.coerce_type value D.key with
      | Keys.Eq -> v

  let ty = ClValue.ty

  let clvalue: t -> ClValue.t = fun x -> x

  let of_clvalue: ClValue.t -> t option = function
    | Cl.Value(_,_,value',_) as v when Value.equal value' D.key -> Some v
    | _ -> None

  let coerce_clvalue: ClValue.t -> t = function
    | Cl.Value(_,_,value',_) as v -> assert (Value.equal value' D.key); v

  let () =
    VValue.inc_size D.key defined_value;
    assert (if not (VValue.is_uninitialized defined_value D.key)
      then raise AlreadyRegisteredKey else true);
    let value = (module D: Value with type t = D.t) in
    VValue.set defined_value D.key value;
    Cl.ValueIndex.set Cl.valueindex D.key (fun v ty -> index v ty)

end


module Print = struct (** Cutting the knot for pp *)
  (* type psem = { mutable psem : 'a. ('a sem -> 'a Pp.pp)} *)

  (* let psem : psem = *)
  (*   {psem = fun _ _ _ -> assert false} (\** called too early *\) *)
  (* let sem sem fmt s = psem.psem sem fmt s *)

  type pdem_event = { mutable
      pdem_event : 'k 'd. ('k,'d) dem -> 'k Pp.pp}

  let pdem_event : pdem_event =
    {pdem_event = fun _ _ _ -> assert false} (** called too early *)
  let dem_event dem fmt s = pdem_event.pdem_event dem fmt s

  type pdem_runable = { mutable
      pdem_runable : 'k 'd. ('k,'d) dem -> 'd Pp.pp}

  let pdem_runable : pdem_runable =
    {pdem_runable = fun _ _ _ -> assert false} (** called too early *)
  let dem_runable dem fmt s = pdem_runable.pdem_runable dem fmt s


end

module Only_for_solver = struct
  type sem_of_cl =
    | Sem: 'a sem * 'a  -> sem_of_cl

  let clsem: Cl.t -> ClSem.t option = function
    | Cl.Fresh _ | Cl.Fresh_to_reg _ | Cl.Value _ -> None
    | Cl.Sem _ as x -> Some (Obj.magic x: ClSem.t)

  let sem_of_cl: ClSem.t -> sem_of_cl = function
    | Cl.Sem (_,_,sem,v) -> Sem(sem,v)

  type value_of_cl =
    | Value: 'a value * 'a  -> value_of_cl

  let clvalue: Cl.t -> ClValue.t option = function
    | Cl.Fresh _ | Cl.Fresh_to_reg _ | Cl.Sem _ -> None
    | Cl.Value _ as x -> Some (Obj.magic x: ClValue.t)

  let value_of_cl: ClValue.t -> value_of_cl = function
    | Cl.Value (_,_,value,v) -> Value(value,v)

  let cl_of_clsem : ClSem.t -> Cl.t = ClSem.cl
  let cl_of_clvalue : ClValue.t -> Cl.t = ClValue.cl

  type opened_cl =
    | Fresh: opened_cl
    | Fresh_to_reg: ('event,'r) dem * 'event -> opened_cl
    | Sem  : ClSem.t -> opened_cl
    | Value  : ClValue.t -> opened_cl

  let open_cl = function
    | Cl.Fresh _ -> Fresh
    | Cl.Fresh_to_reg(_,_,dem,event) -> Fresh_to_reg(dem,event)
    | Cl.Sem _ as x -> Sem (Obj.magic x: ClSem.t)
    | Cl.Value _ as x -> Value (Obj.magic x: ClValue.t)
end


let check_initialization () =
  let well_initialized = ref true in

  Sem.iter {Sem.iter = fun sem ->
    if VSem.is_uninitialized defined_sem sem then begin
      Format.eprintf
        "[Warning] The set of values %a is not registered" Sem.pp sem;
      well_initialized := false;
    end};

  !well_initialized

