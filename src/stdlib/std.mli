open Witan_popop_lib.Stdlib

include module type of Std_sig

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
