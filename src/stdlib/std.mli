include module type of Std_sig

val nnil : 'a list -> bool
  
module Poly : sig

  type (_,_,_) t =
    | Eq : ('a,'a,[< `Eq | `IsEq | `Ord]) t
    | Neq: (_,_,[`IsEq]) t
    | Gt : (_,_,[`Ord]) t
    | Lt : (_,_,[`Ord]) t
          
  type ('a,'b) eq   = ('a,'b,[`Eq]) t
  type ('a,'b) iseq = ('a,'b,[`IsEq]) t
  type ('a,'b) ord  = ('a,'b,[`Ord]) t

  val iseq : ('a,'b,[< `Eq | `IsEq | `Ord]) t -> ('a,'b) iseq

  exception NotEq  
  val eq   : ('a,'b,[< `Eq | `IsEq | `Ord]) t -> ('a,'b) eq
  
end

module Goption : sig
  type (_,_) t =
    | Some: 'a -> ('a,[`Some]) t
    | None:       ('a,[`None]) t
end

module Q : sig
  include module type of Q
  include Witan_popop_lib.Stdlib.Datatype with type t := t
  val two : t
  val ge  : t -> t -> bool
  val le  : t -> t -> bool
  val of_string_decimal : string -> t option
end
