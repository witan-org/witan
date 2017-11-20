module type S = sig

  module Symbols : sig
    exception Unknown
    val equal   : (Sorts.t -> Sorts.t -> bool) -> Symbols.t -> Symbols.t -> bool
    val compare : (Sorts.t -> Sorts.t -> int) -> Symbols.t -> Symbols.t -> int
    val arity   : Symbols.t -> Symbols.arity
    val pp : Format.formatter -> Symbols.t -> unit
  end

  module Sorts : sig
    exception Unknown
    val equal   : (Sorts.t -> Sorts.t -> bool) -> Sorts.t -> Sorts.t -> bool
    val compare : (Sorts.t -> Sorts.t -> int) -> Sorts.t -> Sorts.t -> int
    val pp : (Format.formatter -> Sorts.t -> unit) -> Format.formatter -> Sorts.t -> unit
  end

  module SemanticTerms : sig
    type t [@@deriving eq, ord]
  end  
  
end
