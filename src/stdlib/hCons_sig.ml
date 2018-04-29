(**********************************************************)
(* This file contains the implementation of HConsed types *)
(**********************************************************)

module type PolyArg = sig 
  type ('t,'a) t [@@deriving eq]
  val hash : 't Hash.t -> 'a Hash.t -> ('t,'a) t Hash.t
  val name : string
end

module type PolyS = sig
  type ('t,'a) initial
  type ('a,'dh) generic
  type ('a,'dh) g_revealed = (('a,'dh) generic,'a) initial

  val reveal  : ('a,'d*'h) generic -> ('a,'d*'h) g_revealed
  val data    : ('a,'data*'hcons) generic -> 'data
  val noHCons :
    ('a,'data*[`NoHCons]) g_revealed -> 'data Lazy.t -> ('a,'data*[`NoHCons]) generic
end

module type Arg = sig 
  type 't t [@@deriving eq]
  val hash : 't Hash.t -> 't t Hash.t
  val name : string
end

module type S = sig
  type 't initial
  type 'dh generic
  type 'dh g_revealed = 'dh generic initial
  val reveal  : ('d*'h) generic -> ('d*'h) g_revealed
  val data    : ('d*'h) generic -> 'd
  val noHCons :
    ('data*[`NoHCons]) g_revealed -> 'data Lazy.t -> ('data*[`NoHCons]) generic
end

module type SHCons = sig
  type t [@@deriving eq,ord]
  type revealed
  val id    : t -> int
  val hash  : t Hash.t
  val build : revealed -> t
  val clear : unit -> unit
  (* val backindex: (int -> t,M.backindex) Goption.t *)
end
