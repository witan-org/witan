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

module Map : module type of Extmap
module XHashtbl : Exthtbl.Hashtbl

(* Set, Map, Hashtbl on structures with a unique tag *)

module type TaggedType =
sig
  type t
  val tag : t -> int
  val pp:  t Pp.pp
end

module type OrderedHashedType =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp: t Pp.pp
end

module type Datatype = sig
  include OrderedHashedType

  module M : Map_intf.PMap with type key = t
  module S : Map_intf.Set with type 'a M.t = 'a M.t
                           and type M.key = M.key
  module H : Exthtbl.Hashtbl.S with type key = t
end

module type Printable = sig
  include OrderedHashedType
  val pp: t Pp.pp
end

module MkDatatype(T : OrderedHashedType) : sig
  module M : Map_intf.PMap with type key = T.t
  module S : Map_intf.Set with type 'a M.t = 'a M.t
                           and type M.key = M.key
  module H : Exthtbl.Hashtbl.S with type key = T.t
end

module OrderedHashed (X : TaggedType) :
  OrderedHashedType with type t = X.t

module OrderedHashedList (X : TaggedType) :
  OrderedHashedType with type t = X.t list

module MakeMSH (X : TaggedType) : Datatype with type t = X.t

module MakeMSHW (X : Weakhtbl.Weakey) :
sig
  module M : Map_intf.PMap with type key = X.t
  module S : module type of struct include Extset.MakeOfMap(M) end
  module H : Exthtbl.Hashtbl.S with type key = X.t
  module W : Weakhtbl.S with type key = X.t
end

(* Set, Map, Hashtbl on ints and strings *)

module DInt : Datatype with type t = int
module DIntOrd : Datatype with type t = int

module DStr : Datatype with type t = string
module DFloat : Datatype with type t = float

module DUnit : Datatype with type t = unit
module DBool : Datatype with type t = bool
