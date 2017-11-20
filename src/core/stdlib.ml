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

module Map = Extmap
module XHashtbl = Exthtbl.Hashtbl

(* Set, Map, Hashtbl on structures with a unique tag *)

module type TaggedType =
sig
  type t
  val tag : t -> int
  val pp: t Pp.pp
end

module type OrderedHashedType =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp: t Pp.pp
end

module OrderedHashed (X : TaggedType) =
struct
  type t = X.t
  let hash = X.tag
  let equal ts1 ts2 = X.tag ts1 == X.tag ts2 (** Todo ts1 == ts2? *)
  let compare ts1 ts2 = Pervasives.compare (X.tag ts1) (X.tag ts2)
  let pp = X.pp
end

module OrderedHashedList (X : TaggedType) =
struct
  type t = X.t list
  let hash = Lists.hash X.tag 3
  let equ_ts ts1 ts2 = X.tag ts1 == X.tag ts2
  let equal = Lists.equal equ_ts
  let cmp_ts ts1 ts2 = Pervasives.compare (X.tag ts1) (X.tag ts2)
  let compare = Lists.compare cmp_ts
  let pp = Pp.list Pp.comma X.pp
end

module MakeMSH (X : TaggedType) =
struct
  module T = OrderedHashed(X)
  include T
  module MGen = Intmap.Make(struct
      include X
      let equal ts1 ts2 = X.tag ts1 == X.tag ts2
    end)
  module M = MGen.NT
  module S = Extset.MakeOfMap(M)
  module H = XHashtbl.Make(T)
end

module MakeTagged (X : Weakhtbl.Weakey) =
struct
  type t = X.t
  let tag t = Weakhtbl.tag_hash (X.tag t)
  let pp = X.pp
end

module MakeMSHW (X : Weakhtbl.Weakey) =
struct
  module T = OrderedHashed(MakeTagged(X))
  module M = Map.Make(T)
  module S = Extset.MakeOfMap(M)
  module H = XHashtbl.Make(T)
  module W = Weakhtbl.Make(X)
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

module MkDatatype(T : OrderedHashedType) = struct
  module M = Map.Make(T)
  module S = Extset.MakeOfMap(M)
  module H = XHashtbl.Make(T)
end

(* Set, Map, Hashtbl on ints and strings *)

module Int = struct
  type t = int
  let compare (x : int) (y : int)  = Pervasives.compare x y
  let equal (x : int) y = x = y
  let hash  (x : int) = x
  let tag x = x
  let pp = Pp.int
 end


module DInt = struct
  include Int
  let pp fmt x = Format.pp_print_int fmt x
  module GM  = Intmap.Make(Int)
  module M = GM.NT
  module S = Extset.MakeOfMap(M)
  module H = XHashtbl.Make(Int)
end

module DIntOrd = DInt

module DUnit = Unit

module Bool = struct
  type t = bool
  let compare (x : bool) (y : bool)  = Pervasives.compare x y
  let equal (x : bool) y = x = y
  let hash  (x : bool) = (Obj.magic x : int)
  let pp = Format.pp_print_bool
end

module DBool = struct
  include Bool
  module M = Map.Make(Bool)
  module S = Extset.MakeOfMap(M)
  module H = XHashtbl.Make(Bool)
end

module DStr = struct
  module Str = struct
    type t = String.t
        let compare = String.compare
    let hash    = (Hashtbl.hash : string -> int)
    let equal   = ((=) : string -> string -> bool)
        let pp   = Format.pp_print_string
  end
  include Str
  module M = Map.Make(Str)
  module S = Extset.MakeOfMap(M)
  module H = XHashtbl.Make(Str)
end


module DFloat = struct
module Float = struct
  type t = float
  let compare (x : float) y  = Pervasives.compare x y
  let equal (x : float) y = x = y
  let hash  (x : float) = XHashtbl.hash x
    let pp   = Format.pp_print_float
end
  include Float

  module M = Map.Make(Float)
  module S = Extset.MakeOfMap(M)
  module H = XHashtbl.Make(Float)
end

