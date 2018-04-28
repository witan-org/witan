open Witan_popop_lib.Stdlib

include module type of Std_sig

module Poly : sig

  type (_,_,_) t =
    | Eq : ('a,'a,_) t
    | Neq: (_,_,[`IsEq]) t
    | Gt : (_,_,[`Ord]) t
    | Lt : (_,_,[`Ord]) t
          
  type ('a,'b) eq   = ('a,'b,[`Eq]) t
  type ('a,'b) iseq = ('a,'b,[`IsEq]) t
  type ('a,'b) ord  = ('a,'b,[`Ord]) t

  val iseq : ('a,'b,_) t -> ('a,'b) iseq

  exception NotEq  
  val eq   : ('a,'b,_) t -> ('a,'b) eq
  
end

module Goption : sig
  type (_,_) t =
    | Some: 'a -> ('a,[`Some]) t
    | None:       ('a,[`None]) t
end

module Q : sig
  include module type of Q
  include Datatype with type t := Q.t
  val hash : 'a -> int
  val pp : Containers.Format.t -> Q.t -> unit
  val le : t -> t -> bool
  val ge : t -> t -> bool
  val two : Q.t
  val of_string_decimal : string -> Q.t option
end
